// Copyright 2017-2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0

#if defined(__unix__) || defined(__APPLE__)
#include "platform.hh"

#include "utils.hh"
#include "log.hh"

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h> // required for stat.h
#include <sys/wait.h>
#include <sys/prctl.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdarg.h>
#ifdef __GLIBC__
#include <malloc.h>
#endif

#include <llvm/ADT/SmallString.h>
#include <llvm/Support/Path.h>

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <string>

namespace ccls {
namespace pipeline {
void threadEnter();
}

std::string normalizePath(llvm::StringRef path) {
  llvm::SmallString<256> p(path);
  llvm::sys::path::remove_dots(p, true);
  return {p.data(), p.size()};
}

void freeUnusedMemory() {
#ifdef __GLIBC__
  malloc_trim(4 * 1024 * 1024);
#endif
}

void traceMe() {
  // If the environment variable is defined, wait for a debugger.
  // In gdb, you need to invoke `signal SIGCONT` if you want ccls to continue
  // after detaching.
  const char *traceme = getenv("CCLS_TRACEME");
  if (traceme)
    raise(traceme[0] == 's' ? SIGSTOP : SIGTSTP);
}

void spawnThread(void *(*fn)(void *), void *arg) {
  pthread_t thd;
  pthread_attr_t attr;
  struct rlimit rlim;
  size_t stack_size = 4 * 1024 * 1024;
  if (getrlimit(RLIMIT_STACK, &rlim) == 0 && rlim.rlim_cur != RLIM_INFINITY)
    stack_size = rlim.rlim_cur;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&attr, stack_size);
  pipeline::threadEnter();
  pthread_create(&thd, &attr, fn, arg);
  pthread_attr_destroy(&attr);
}


void Process::start(const char* name, char *const argv[])
{
  const size_t Read = 0;
  const size_t Write = 1;
  int inputPipeFD[2] = {};
  int outputPipeFD[2] = {};
  socketpair(PF_LOCAL, SOCK_STREAM, AF_LOCAL, inputPipeFD);
  socketpair(PF_LOCAL, SOCK_STREAM, AF_LOCAL, outputPipeFD);
  int new_pid = fork();
  if (new_pid == 0)
  {
    // Child
    close(inputPipeFD[Read]);
    close(outputPipeFD[Write]);
    dup2(inputPipeFD[Write], STDOUT_FILENO);
    dup2(outputPipeFD[Read], STDIN_FILENO);

    //ask kernel to deliver SIGTERM in case the parent dies
    prctl(PR_SET_PDEATHSIG, SIGTERM);

    //replace tee with your process
    int res = execv(name, argv);
    if (res < 0) {
      const char* err = strerror(errno);
      fprintf(stderr, "Failed to start %s\n", name);
      fprintf(stderr, "    %s\n", err);
    }
    // Nothing below this line should be executed by child process. If so, 
    // it means that the execl function wasn't successfull, so lets exit:
    exit(1);
  }
  // The code below will be executed only by parent. You can write and read
  // from the child using pipefd descriptors, and you can send signals to 
  // the process using its pid by kill() function. If the child process will
  // exit unexpectedly, the parent process will obtain SIGCHLD signal that
  // can be handled (e.g. you can respawn the child process).

  //close unused pipe ends
  close(inputPipeFD[Write]);
  close(outputPipeFD[Read]);
  pid = new_pid;
  if (new_pid > 0) {
    inputFD = inputPipeFD[Read];
    outputFD = outputPipeFD[Write];
  } else {
    close(inputPipeFD[Read]);
    close(outputPipeFD[Write]);
    inputFD = -1;
    outputFD = -1;
  }
  LOG_S(INFO) << "Started fzf process";
}

Process::~Process() {
  if (inputFD >= 0)
    close(inputFD);
  if (outputFD >= 0)
    close(outputFD);

  LOG_S(INFO) << "Stopping fzf process";
  waitpid(pid, NULL, 0);
  LOG_S(INFO) << "Stopped fzf process"; 
}

const std::string_view Process::read() {
  if (inputFD < 0)
    return {};
  currentString.clear();
  auto ret = readString();
  while(ret.empty())
  {
    int bytesRead = 0;
    readBuffer.resize(BufferSize);
    bufferPos = 0;
    bytesRead = ::read(inputFD, readBuffer.data(), readBuffer.size());
    LOG_S(INFO) << "Read " << bytesRead << " bytes";
    if (bytesRead <= 0) {
      return {}; 
    }
    readBuffer.resize(bytesRead);
    auto view = std::string_view(readBuffer.data(), readBuffer.size());
    LOG_S(INFO) << view << "end view";

    ret = readString();
  }
  return ret;
}

const std::string_view Process::readString() {
  auto startPos = bufferPos;
  for (;bufferPos<readBuffer.size(); bufferPos++) {
    if (readBuffer[bufferPos] == 0) {
      auto view = std::string_view{readBuffer.data() + startPos,  bufferPos - startPos};
      bufferPos++;
      if (currentString.empty()) {
        return view;
      } else {
        currentString.append(view);
        return currentString;
      }
    }
  }
  currentString.append(readBuffer.data() + startPos, readBuffer.size() - startPos);
  return {};
}

void Process::write(const std::string_view& buffer) {
    if (outputFD >= 0) {
      // +1 for the null terminator
      send(outputFD, buffer.data(), buffer.size(), MSG_NOSIGNAL);
      const char nullTerm = 0;
      send(outputFD, &nullTerm, 1, MSG_NOSIGNAL);
    }
}

void Process::closeWrite() {
  close(outputFD);
  outputFD = -1;
}


} // namespace ccls

#endif
