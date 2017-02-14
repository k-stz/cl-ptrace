#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>


// ptrace prototype/signature:
// #include <sys/ptrace.h>
// long int ptrace(enum __ptrace_request request, pid_t pid,
//                 void *addr, void *data)

int child_do(int argc, char **argv);
int tracer_do(void);

pid_t child;


int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: ./ptrace-test.c <program> <optional argumens>\n");
  }

  child = fork();
  if (child == 0) {
    // wow, argv+1 means that the passed string array will start at an offset of 1,
    // such that argv[1] becomes argv[0] in the child_do passed argument
    return child_do(argc-1, argv+1); 
  } else {
  } if (child != 0) {
    return tracer_do();
  }
}


int child_do(int argc, char **argv) {
  char *args[argc+1]; // because execvp wants a NULL terminated array
  args[argc] = NULL;  // and here we place the NULL
  memcpy(args, argv, argc * sizeof(char*));
  ptrace(PTRACE_TRACEME, 0, NULL, NULL);
  kill(getpid(), SIGSTOP);
  return execvp(args[0], args);
}

int tracer_do(void) {
  int status;
  printf("Parent called: %d\n", getpid());
  waitpid(child, &status, 0); // waiting for the child to finish
  // PEEKUSER: read memory from offset, offset is typically word alligned
  int x = ptrace(PTRACE_PEEKUSER, child, 0);
  int y = ptrace(PTRACE_PEEKUSER, child, 1);
  int z = ptrace(PTRACE_PEEKUSER, child, 2);
  int w = ptrace(PTRACE_PEEKUSER, child, 3);

  // GETREGS reads from, usually, general purpose registers (SETREGS..) sets them
  printf("  peek: %d %d %d %d\n", x, y ,z, w);

  printf("Child termination status: %d\n", status);
  printf("Child return val:%d\n", WEXITSTATUS(status));
  
  return 0;
}

