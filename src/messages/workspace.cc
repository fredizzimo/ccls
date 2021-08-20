// Copyright 2017-2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0

#include "fuzzy_match.hh"
#include "log.hh"
#include "message_handler.hh"
#include "pipeline.hh"
#include "project.hh"
#include "query.hh"
#include "sema_manager.hh"
extern "C" {
#include "fzf.h"
}

#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Path.h>

#include <algorithm>
#include <ctype.h>
#include <functional>
#include <limits.h>
#include <memory>
using namespace llvm;

namespace ccls {
REFLECT_STRUCT(SymbolInformation, name, kind, location, containerName);

void MessageHandler::workspace_didChangeConfiguration(EmptyParam &) {
  for (auto &[folder, _] : g_config->workspaceFolders)
    project->load(folder);
  project->index(wfiles, RequestId());

  manager->clear();
};

void MessageHandler::workspace_didChangeWatchedFiles(
    DidChangeWatchedFilesParam &param) {
  for (auto &event : param.changes) {
    std::string path = event.uri.getPath();
    if ((g_config->cache.directory.size() &&
         StringRef(path).startswith(g_config->cache.directory)) ||
        lookupExtension(path).first == LanguageId::Unknown)
      return;
    for (std::string cur = path; cur.size(); cur = sys::path::parent_path(cur))
      if (cur[0] == '.')
        return;

    switch (event.type) {
    case FileChangeType::Created:
    case FileChangeType::Changed: {
      IndexMode mode =
          wfiles->getFile(path) ? IndexMode::Normal : IndexMode::Background;
      pipeline::index(path, {}, mode, true);
      if (event.type == FileChangeType::Changed) {
        if (mode == IndexMode::Normal)
          manager->onSave(path);
        else
          manager->onClose(path);
      }
      break;
    }
    case FileChangeType::Deleted:
      pipeline::index(path, {}, IndexMode::Delete, false);
      manager->onClose(path);
      break;
    }
  }
}

void MessageHandler::workspace_didChangeWorkspaceFolders(
    DidChangeWorkspaceFoldersParam &param) {
  for (const WorkspaceFolder &wf : param.event.removed) {
    std::string root = wf.uri.getPath();
    ensureEndsInSlash(root);
    LOG_S(INFO) << "delete workspace folder " << wf.name << ": " << root;
    auto it = llvm::find_if(g_config->workspaceFolders,
                            [&](auto &folder) { return folder.first == root; });
    if (it != g_config->workspaceFolders.end()) {
      g_config->workspaceFolders.erase(it);
      {
        // auto &folder = project->root2folder[path];
        // FIXME delete
      }
      project->root2folder.erase(root);
    }
  }
  auto &workspaceFolders = g_config->workspaceFolders;
  for (const WorkspaceFolder &wf : param.event.added) {
    std::string folder = wf.uri.getPath();
    ensureEndsInSlash(folder);
    std::string real = realPath(folder) + '/';
    if (folder == real)
      real.clear();
    LOG_S(INFO) << "add workspace folder " << wf.name << ": "
                << (real.empty() ? folder : (folder + " -> ").append(real));
    workspaceFolders.emplace_back();
    auto it = workspaceFolders.end() - 1;
    for (; it != workspaceFolders.begin() && folder < it[-1].first; --it)
      *it = it[-1];
    *it = {folder, real};
    project->load(folder);
  }

  project->index(wfiles, RequestId());

  manager->clear();
}

namespace {
// Lookup |symbol| in |db| and insert the value into |result|.
template <typename Adder>
bool addSymbol(DB *db, WorkingFiles *wfiles,
               const std::vector<uint8_t> &file_set, SymbolIdx sym,
               Adder adder) {
  std::optional<SymbolInformation> info = getSymbolInfo(db, sym, true);
  if (!info)
    return false;

  Maybe<DeclRef> dr;
  bool in_folder = false;
  withEntity(db, sym, [&](const auto &entity) {
    for (auto &def : entity.def)
      if (def.spell) {
        dr = def.spell;
        if (!in_folder && (in_folder = file_set[def.spell->file_id]))
          break;
      }
  });
  if (!dr) {
    auto &decls = getNonDefDeclarations(db, sym);
    for (auto &dr1 : decls) {
      dr = dr1;
      if (!in_folder && (in_folder = file_set[dr1.file_id]))
        break;
    }
  }
  if (!in_folder)
    return false;

  std::optional<Location> ls_location = getLsLocation(db, wfiles, *dr);
  if (!ls_location)
    return false;
  info->location = *ls_location;
  adder(std::move(*info));
  return true;
}

template <typename RandomIt, typename Compare>
void sift_down(RandomIt begin, RandomIt end, const Compare &comp = {}) {
  const auto length = static_cast<size_t>(end - begin);
  size_t current = 0;
  size_t next = 2;
  while (next < length) {
    if (comp(*(begin + next), *(begin + (next - 1))))
      --next;
    if (!comp(*(begin + current), *(begin + next)))
      return;
    std::iter_swap(begin + current, begin + next);
    current = next;
    next = 2 * current + 2;
  }
  --next;
  if (next < length && comp(*(begin + current), *(begin + next)))
    std::iter_swap(begin + current, begin + next);
}

template <typename T, typename Deleter>
decltype(auto) make_unique(T *p, Deleter deleter) {
  return std::unique_ptr<T, Deleter>(p, deleter);
}

void workspace_symbol_fzf(MessageHandler &handler, const std::string &query,
                          const std::vector<uint8_t> &file_set,
                          ReplyOnce &reply) {

  const fzf_case_types case_modes[] = {case_ignore, case_smart, case_respect};
  fzf_case_types case_mode =
      case_modes[g_config->workspaceSymbol.caseSensitivity];
  const bool normalize = false;
  const bool fuzzy = true;
  fzf_string_t fzf_query = {query.data(), query.size()};
  auto fzf_pattern = make_unique(
      fzf_parse_pattern_str(case_mode, normalize, &fzf_query, fuzzy),
      [](auto *p) { fzf_free_pattern(p); });
  auto slab =
      make_unique(fzf_make_default_slab(), [](auto *p) { fzf_free_slab(p); });

  auto *db = handler.db;
  const size_t max_num = std::min<size_t>(g_config->workspaceSymbol.maxNum,
                                          std::numeric_limits<uint32_t>::max());
  std::vector<SymbolInformation> cands;
  std::vector<std::tuple<int32_t, uint32_t>> cands_heap;
  cands.reserve(max_num);
  cands_heap.reserve(max_num);

  auto add = [&](SymbolIdx sym) {
    std::string_view detailed_name = handler.db->getSymbolName(sym, true);
    if (detailed_name.size() > 0) {
      fzf_string_t name = {detailed_name.data(), detailed_name.size()};
      auto score = fzf_get_score_str(&name, fzf_pattern.get(), slab.get());
      if (score > 0) {
        if (cands.size() < max_num) {
          addSymbol(db, handler.wfiles, file_set, sym,
                    [&cands, &cands_heap, &score](SymbolInformation &&info) {
                      cands_heap.emplace_back(score, cands.size());
                      cands.emplace_back(std::move(info));
                    });
        } else {
          if (cands.size() == max_num) {
            std::make_heap(cands_heap.begin(), cands_heap.end(),
                           [](const auto &lhs, const auto &rhs) {
                             return std::get<0>(lhs) > std::get<0>(rhs);
                           });
          }
          auto &max_element = cands_heap[0];
          auto &max_score = std::get<0>(max_element);
          if (score > max_score) {
            addSymbol(db, handler.wfiles, file_set, sym,
                      [&](SymbolInformation &&info) {
                        max_score = score;
                        auto &sym = cands[std::get<1>(max_element)];
                        sym = std::move(info);
                        sift_down(cands_heap.begin(), cands_heap.end(),
                                  [&](const auto &lhs, const auto &rhs) {
                                    const auto &sl = std::get<0>(lhs);
                                    const auto &sr = std::get<0>(rhs);
                                    return sl > sr;
                                  });
                      });
          }
        }
      }
    }
  };
  for (auto &func : db->funcs)
    add({func.usr, Kind::Func});
  for (auto &type : db->types)
    add({type.usr, Kind::Type});
  for (auto &var : db->vars)
    if (var.def.size() && !var.def[0].is_local())
      add({var.usr, Kind::Var});

  std::sort(cands_heap.begin(), cands_heap.end(),
            [](const auto &l, const auto &r) { return l > r; });
  std::vector<SymbolInformation> result;
  result.reserve(cands_heap.size());
  for (const auto &i : cands_heap) {
    auto &info = cands[std::get<1>(i)];
    result.push_back(std::move(info));
  }

  reply(result);
}
} // namespace

void MessageHandler::workspace_symbol(WorkspaceSymbolParam &param,
                                      ReplyOnce &reply) {
  std::vector<SymbolInformation> result;
  const std::string &query = param.query;
  for (auto &folder : param.folders)
    ensureEndsInSlash(folder);
  std::vector<uint8_t> file_set = db->getFileSet(param.folders);
  const bool use_fzf = g_config->workspaceSymbol.sort;
  if (use_fzf)
    return workspace_symbol_fzf(*this, query, file_set, reply);

  // {symbol info, matching detailed_name or short_name, index}
  std::vector<std::tuple<SymbolInformation, int, SymbolIdx>> cands;
  bool sensitive = g_config->workspaceSymbol.caseSensitivity;

  // Find subsequence matches.
  std::string query_without_space;
  query_without_space.reserve(query.size());
  for (char c : query)
    if (!isspace(c))
      query_without_space += c;

  auto add = [&](SymbolIdx sym) {
    std::string_view detailed_name = db->getSymbolName(sym, true);
    int pos = reverseSubseqMatch(query_without_space, detailed_name, sensitive);
    return pos >= 0 &&
           addSymbol(db, wfiles, file_set, sym,
                     [&](SymbolInformation &&info) {
                       const bool detailed =
                           detailed_name.find(':', pos) != std::string::npos;
                       cands.emplace_back(std::move(info), detailed, sym);
                     }) &&
           cands.size() >= g_config->workspaceSymbol.maxNum;
  };
  for (auto &func : db->funcs)
    if (add({func.usr, Kind::Func}))
      goto done_add;
  for (auto &type : db->types)
    if (add({type.usr, Kind::Type}))
      goto done_add;
  for (auto &var : db->vars)
    if (var.def.size() && !var.def[0].is_local() && add({var.usr, Kind::Var}))
      goto done_add;
done_add:
  if (g_config->workspaceSymbol.sort &&
      (query.size() <= FuzzyMatcher::kMaxPat)) {
    // Sort results with a fuzzy matching algorithm.
    int longest = 0;
    for (auto &cand : cands)
      longest = std::max(
          longest, int(db->getSymbolName(std::get<2>(cand), true).size()));
    FuzzyMatcher fuzzy(query, g_config->workspaceSymbol.caseSensitivity);
    for (auto &cand : cands)
      std::get<1>(cand) = fuzzy.match(
          db->getSymbolName(std::get<2>(cand), std::get<1>(cand)), false);
    std::sort(cands.begin(), cands.end(), [](const auto &l, const auto &r) {
      return std::get<1>(l) > std::get<1>(r);
    });
    result.reserve(cands.size());
    for (auto &cand : cands) {
      // Discard awful candidates.
      if (std::get<1>(cand) <= FuzzyMatcher::kMinScore)
        break;
      result.push_back(std::get<0>(cand));
    }
  } else {
    result.reserve(cands.size());
    for (auto &cand : cands)
      result.push_back(std::get<0>(cand));
  }

  reply(result);
}
} // namespace ccls
