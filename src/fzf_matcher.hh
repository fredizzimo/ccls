// Copyright 2017-2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0

#pragma once
#include <string_view>
#include "fzf.h"

class FzfMatcher {
  public:
    FzfMatcher(std::string_view pattern);
    ~FzfMatcher();
    fzf_result* match(const char** lines, uint32_t* lineLengths, size_t numLines);
  private:
    fzf_pattern* pattern = nullptr;
};
