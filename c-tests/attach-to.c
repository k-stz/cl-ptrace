// including header files,
// is sematically the same as posting the contents of its belonging file here. #include is
// a preprocessor directive that takes care of that for us, it searches for a given
// argument header in a standard list of system directories.  You can add directories to
// this list with the -I option while compiling the source code
// On my machine the files can be found in /usr/include for example sys/user.h is in
// /usr/include/sys/user.h
// on another machine in /usr/include/x86_64-linux-gnu/sys/ptrace.h

#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/user.h> // for: struct user_regs_struct !

#include "util.h"
#include "util.c"

// ptrace prototype/signature:
// #include <sys/ptrace.h>
// long int ptrace(enum __ptrace_request request, pid_t pid,
//                 void *addr, void *data)


pid_t tracee_pid;

struct user_regs_struct regs;

int status;
bool keep_looping = true;

int main(int argc, char **argv) {
  print_endianness();
  if (argc < 2) {
    printf("Usage: ./attach-to.c <pid-of-target-process>\n");
  } else {
    tracee_pid = atoi(argv[1]);
    printf("REACHED pid:%d \n", tracee_pid);
    // process will be stopped
    ptrace(PTRACE_ATTACH, tracee_pid, NULL, NULL);
    // according to the man page the waitpid ensures that we proceed only with the process
    // stopped:
    waitpid(tracee_pid, &status, 0);
    printf("PTRACE_ATTACH waitpid status: %d \n", status);

    printf("now lets step through the child\n");
    printf("Press ENTER to step or q and then ENTER to quit\n");
    while( keep_looping == true) {
      ptrace(PTRACE_SINGLESTEP, tracee_pid, NULL, NULL);
      waitpid(tracee_pid, &status, 0);
      ptrace(PTRACE_GETREGS, tracee_pid, NULL, &regs);
      // thanks to waitpid the code here now definitely deals with a stopped child at its
      // next instruction:
      printf("Current Register state:\n");
      print_user_regs_struct (regs);

      keep_looping = peekpoke_interactively(tracee_pid, regs);
      

    }
    printf("singlestepping aborted.\n");
    ptrace(PTRACE_DETACH, tracee_pid, NULL, NULL);

  };

}

