// Copyright 2017-2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0

#pragma once
#include <string_view>
#include "fzf.h"

class FzfMatcher {
  public:
    FzfMatcher(std::string_view pattern);
    ~FzfMatcher();
    bool match(std::string_view line);
  private:
    fzf_pattern* pattern = nullptr;
};
