// Copyright 2017-2020 ccls Authors
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <llvm/ADT/StringRef.h>

#include <string>
#include <string_view>

namespace ccls {
std::string normalizePath(llvm::StringRef path);

// Free any unused memory and return it to the system.
void freeUnusedMemory();

// Stop self and wait for SIGCONT.
void traceMe();

void spawnThread(void *(*fn)(void *), void *arg);

class Process {
public:
    template<typename ...T>
    Process(T... args)
    {
        char *const argv[] = {const_cast<char*>(args)..., nullptr};
        start(argv[0], argv);
    }
    ~Process();

    const std::string_view read();
    void write(const std::string_view& buffer);
    void closeWrite();
private:
    static constexpr size_t BufferSize = 4096;
    void start(const char* name, char *const argv[]);
    const std::string_view readString();
    std::string currentString;
    std::vector<char> readBuffer;
    size_t bufferPos = 0;
    int pid = 0;
    int inputFD = -1;
    int outputFD = -1;
};

} // namespace ccls
