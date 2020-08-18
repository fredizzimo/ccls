// Copyright 2017-2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0

#include "fzf_matcher.hh"
#include "dlfcn.h"
#include "log.hh"

namespace {
  
  decltype(&fzf_make_pattern) FzfMakePattern;
  decltype(&fzf_free_pattern) FzfFreePattern;
  decltype(&fzf_match) FzfMatch;

  bool loadLibrary() {
    static void* lib = nullptr;
    const char* path = "/home/fredizzimo/go/src/github.com/junegunn/fzf/target/fzf-linux_amd64.so";
    const auto error = [&](){
        LOG_S(ERROR) << "Failed to load dynamic library " << path;
        LOG_S(ERROR) << dlerror();
        return false;
    };
    if (!lib) {
      lib = dlopen(path, RTLD_NOW);
      if (!lib) {
        return error();
      }
      FzfMakePattern = reinterpret_cast<decltype(&fzf_make_pattern)>(dlsym(lib, "fzf_make_pattern"));
      if (!FzfMakePattern)
        return error();
      FzfFreePattern = reinterpret_cast<decltype(&fzf_free_pattern)>(dlsym(lib, "fzf_free_pattern"));
      if (!FzfFreePattern)
        return error();
      FzfMatch = reinterpret_cast<decltype(&fzf_match)>(dlsym(lib, "fzf_match"));
      if (!FzfMatch)
        return error();
    }
    return true;
  }
}

FzfMatcher::FzfMatcher(std::string_view pattern) {
  if (loadLibrary()) {
    this->pattern = FzfMakePattern((char*)pattern.data(), (int)pattern.size(), FZF_ALGORITHM_FUZZY_V2,
      true, FZF_CASE_SMART, false, FZF_SORT_NONE, FZF_SORT_NONE, FZF_SORT_NONE); 
    if (this->pattern->error != nullptr) {
      LOG_S(ERROR) << this->pattern->error;
      FzfFreePattern(this->pattern);
      this->pattern = nullptr;
    }
  }
}

FzfMatcher::~FzfMatcher() {
  if (pattern) {
    FzfFreePattern(pattern);
  }
}

bool FzfMatcher::match(std::string_view line) {
  if (!pattern) {
    return false;
  }
  return FzfMatch(pattern, (char*)line.data(), (int)line.size());
}
