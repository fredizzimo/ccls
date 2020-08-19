/*
Package fzf implements fzf, a command-line fuzzy finder.

The MIT License (MIT)

Copyright (c) 2020 Fred Sundvik

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

typedef enum fzf_algorithm {
    FZF_ALGORITHM_DEFAULT,
    FZF_ALGORITHM_FUZZY_V1,
    FZF_ALGORITHM_FUZZY_V2,
    FZF_ALGORITHM_EXACT,
} fzf_algorithm;

typedef enum fzf_case {
    FZF_CASE_SMART,
    FZF_CASE_RESPECT,
    FZF_CASE_IGNORE,
} fzf_case;

typedef enum fzf_literal {
    FZF_LITERAL_SMART,
    FZF_LITRAL_RESPECT,
} fzf_literal;

typedef enum fzf_sort {
    FZF_SORT_NONE,
    FZF_SORT_LENGTH,
    FZF_SORT_BEGIN,
    FZF_SORT_END,
    FZF_SORT_INDEX,
} fzf_sort;

typedef struct fzf_pattern {
    uint64_t handle;
    const char* error;
} fzf_pattern;

typedef struct fzf_result {
    uint64_t* scores;
    uint32_t* indices;
    uint32_t num_results;
} fzf_result;

#ifdef __cplusplus
extern "C" {
#endif

extern fzf_pattern* fzf_make_pattern(const char* pattern, uint32_t pattern_length, enum fzf_algorithm algorithm, bool extended,
        enum fzf_case case_mode, bool normalize, enum fzf_sort sort1, enum fzf_sort sort2, enum fzf_sort sort3);
extern void fzf_free_pattern(fzf_pattern* pattern);
extern fzf_result* fzf_match(fzf_pattern* pattern, const char** lines, uint32_t* line_lengths, uint32_t num_lines);

#ifdef __cplusplus
}
#endif
