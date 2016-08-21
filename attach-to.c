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


// ptrace prototype/signature:
// #include <sys/ptrace.h>
// long int ptrace(enum __ptrace_request request, pid_t pid,
//                 void *addr, void *data)


// TODO: test:
//       struct user_regs_struct regs;
//       ptrace(PTRACE_GETREGS, target_process, NULL, &regs); !

pid_t target_process;

char input_char;

struct user_regs_struct regs;

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: ./attach-to.c <pid-of-target-process>\n");
  } else {
    target_process = atoi(argv[1]);
    printf("REACHED pid:%d \n", target_process);
    // process will be stopped
    ptrace(PTRACE_ATTACH, target_process, NULL, NULL);
    printf("Press Enter to continue the process..");
    getchar();
    // continue process
    ptrace(PTRACE_CONT, target_process, NULL, NULL);
    // kill(SIGCONT, target_process); <- won't work

    printf("now lets step through the child\n");
    printf("Press ENTER to step or q and then ENTER to quit\n");
    /* kill(SIGSTOP, target_process); */
    // TODO: doesn't work
    /* while( input_char != 'q') { */
    /*   ptrace(PTRACE_SINGLESTEP, target_process, NULL, NULL); */
    /*   waitpid(target_process, NULL, 0); */
    /*   printf("reached?\n"); */
    /*   input_char = getchar(); */
    /* } */
    printf("singlestepping aborted.\n");
    // ptrace(PTRACE_DETACH, target_process, NULL, NULL);

  };

}


