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
REFLECT_STRUCT(SymbolInformation, name, kind, location, containerName,
               highlights);

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
               bool detailed, Adder adder) {
  std::optional<SymbolInformation> info = getSymbolInfo(db, sym, detailed);
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

auto parse_query(const std::string &query) {
  std::string output;
  bool detailed = false;
  static constexpr std::string_view str_detailed{"d"};
  output.reserve(query.size());
  for (auto i = query.begin(); i != query.end(); ++i) {
    if (*i != '\\') {
      output += *i;
    } else {
      std::string_view view{i + 1, query.end()};
      if (view.starts_with(str_detailed)) {
        detailed = true;
        i += str_detailed.size();
      } else {
        output += *i;
      }
    }
  }
  return std::make_tuple(std::move(output), detailed);
}

std::string_view add_one_qualifier(const std::string_view &name,
                                   const std::string_view &qualified_name) {
  std::string_view temp{qualified_name.begin(),
                        qualified_name.size() - name.size()};
  auto i = temp.find_last_not_of(':');
  if (i != std::string::npos) {
    temp.remove_suffix(temp.size() - i - 1);
    if (temp.ends_with('>')) {
      size_t count = 1;
      temp.remove_suffix(1);
      while (count > 0 && !temp.empty()) {
        char c = temp.back();
        if (c == '>') {
          count++;
        } else if (c == '<') {
          count--;
        }
        temp.remove_suffix(1);
      }
    }
    i = temp.find_last_of(':');
    if (i != std::string::npos) {
      temp.remove_prefix(i + 1);
    }
    size_t diff = name.data() - temp.data();
    return {temp.data(), name.size() + diff};
  } else {
    return name;
  }
}

void workspace_symbol_fzf(MessageHandler &handler, const std::string &query,
                          const std::vector<uint8_t> &file_set,
                          ReplyOnce &reply) {

  const fzf_case_types case_modes[] = {case_ignore, case_smart, case_respect};
  fzf_case_types case_mode =
      case_modes[g_config->workspaceSymbol.caseSensitivity];
  const bool normalize = false;
  const bool fuzzy = true;
  const auto [parsed_query, d] = parse_query(query);
  // The current version of clang does not implement P1091R3 completely
  // So introduce a variable to let the lamba capture work
  auto detailed = d;
  fzf_string_t fzf_query = {parsed_query.data(), parsed_query.size()};
  auto fzf_pattern = make_unique(
      fzf_parse_pattern_str(case_mode, normalize, &fzf_query, fuzzy),
      [](auto *p) { fzf_free_pattern(p); });
  auto slab =
      make_unique(fzf_make_default_slab(), [](auto *p) { fzf_free_slab(p); });

  auto *db = handler.db;
  const size_t max_num = std::min<size_t>(g_config->workspaceSymbol.maxNum,
                                          std::numeric_limits<uint32_t>::max());
  std::vector<std::tuple<SymbolInformation, SymbolIdx>> cands;
  std::vector<std::tuple<int32_t, uint32_t>> cands_heap;
  cands.reserve(max_num);
  cands_heap.reserve(max_num);

  const int8_t unqualified_match_score = 32;
  const int8_t qualified_match_score = 16;
  const int8_t detail_match_score = 8;

  fzf_score_t scoring;
  scoring.score_match_pos = NULL, scoring.score_gap_start = -3,
  scoring.score_gap_extention = -1, scoring.bonus_boundary = 8,
  scoring.bonus_non_word = 8, scoring.bonus_camel_123 = 8 - 1,
  scoring.bonus_consecutive = 3 + 1, scoring.bonus_first_char_multiplier = 1;
  std::vector<int8_t> match_scores;
  const bool is_nested_name = parsed_query.ends_with(':');
  auto call_fzf = [&](SymbolIdx sym, auto f) {
    std::string_view qualified_name = handler.db->getSymbolName(sym, true);
    std::string_view unqualified_name = handler.db->getSymbolName(sym, false);
    std::string_view detailed_name =
        detailed ? handler.db->getDetailedSymbolName(sym) : qualified_name;
    match_scores.resize(detailed_name.size());

    auto set_score = [&](const auto &substr, auto score) {
      size_t offset = substr.data() - detailed_name.data();
      std::fill_n(match_scores.begin() + offset, substr.size(), score);
    };

    set_score(detailed_name, detail_match_score);
    set_score(qualified_name, qualified_match_score);

    if (is_nested_name && qualified_name.data() != unqualified_name.data()) {
      unqualified_name = add_one_qualifier(unqualified_name, qualified_name);
    }
    set_score(unqualified_name, unqualified_match_score);

    scoring.score_match_pos = match_scores.data();
    fzf_string_t name = {detailed_name.data(), detailed_name.size()};
    return f(&name, fzf_pattern.get(), &scoring, slab.get());
  };

  auto add = [&](SymbolIdx sym) {
    if (handler.db->getSymbolName(sym, false).size() > 0) {
      auto score = call_fzf(sym, fzf_get_score_str);
      if (score > 0) {
        if (cands.size() < max_num) {
          addSymbol(
              db, handler.wfiles, file_set, sym, detailed,
              [&cands, &cands_heap, &score, sym](SymbolInformation &&info) {
                cands_heap.emplace_back(score, cands.size());
                cands.emplace_back(std::move(info), sym);
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
            addSymbol(db, handler.wfiles, file_set, sym, detailed,
                      [&](SymbolInformation &&info) {
                        max_score = score;
                        auto &cand = cands[std::get<1>(max_element)];
                        std::get<0>(cand) = std::move(info);
                        std::get<1>(cand) = sym;
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
            [&cands](const auto &lhs, const auto &rhs) {
              const auto &lhs_score = std::get<0>(lhs);
              const auto &rhs_score = std::get<0>(rhs);
              if (lhs_score == rhs_score) {
                const auto &lhs_idx = std::get<1>(lhs);
                const auto &rhs_idx = std::get<1>(rhs);
                const auto &lhs_sym = cands[lhs_idx];
                const auto &rhs_sym = cands[rhs_idx];
                const auto &lhs_info = std::get<0>(lhs_sym);
                const auto &rhs_info = std::get<0>(rhs_sym);
                return lhs_info.name < rhs_info.name;
              } else {
                return lhs_score > rhs_score;
              }
            });
  std::vector<SymbolInformation> result;
  result.reserve(cands_heap.size());
  size_t num_positions = fzf_get_num_positions(fzf_pattern.get());
  num_positions *= cands_heap.size();
  auto position_buffer =
      std::make_unique_for_overwrite<uint32_t[]>(num_positions);
  fzf_position_t pos;
  pos.data = position_buffer.get();
  pos.cap = num_positions;
  for (const auto &i : cands_heap) {
    auto &cand = cands[std::get<1>(i)];
    const auto &info = std::get<0>(cand);
    auto &sym = std::get<1>(cand);
    call_fzf(sym, [&pos](auto *text, auto *pattern, auto *scoring,
                         auto *slab) mutable {
      fzf_get_positions_str(text, pattern, scoring, &pos, slab);
    });

    result.push_back(std::move(info));
    result.back().highlights.emplace(pos.data, pos.size);
    pos.cap -= pos.size;
    pos.data += pos.size;
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
           addSymbol(db, wfiles, file_set, sym, true,
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
