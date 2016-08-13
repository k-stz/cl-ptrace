#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>


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
  return execvp(args[0], args);
}

int tracer_do(void) {
  int status;
  printf("Parent called: %d\n", getpid());
  waitpid(child, &status, 0); // waiting for the child to finish
  printf("Child termination status: %d\n", status);
  printf("Child return val:%d\n", WEXITSTATUS(status));
  
  return 0;
}

