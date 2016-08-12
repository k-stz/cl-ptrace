#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>


int child_do(void);
int tracer_do(void);

pid_t child;


int main(int argc, char **argv) {
  if (argc < 2) {
    printf("argc: %d", argc);
  }
  printf("aaaaaaaaaaaaaaa\n");
  child = fork();
  if (child == 0) {
    child_do();
  } else {
  } if (child != 0) {
    tracer_do();
  }
  exit(0);
}


int child_do(void) {
  printf("Child called: %d\n", getpid());
  ptrace(PTRACE_TRACEME, 0, NULL, NULL);
  return 42;
  
  
}

int tracer_do(void) {
  int status;
  printf("Parent called: %d\n", getpid());
  waitpid(child, &status, 0); // waiting for the child to finish
  printf("Child termination status: %d\n", status);
  printf("Child return val:%d\n", WEXITSTATUS(status));
  return 1;
}
