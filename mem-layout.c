#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// to find the header for the usual systemcall functions use
// man 2 <function-name> !
#include <unistd.h> // sbrk(0)


void waitForInput ();

// trying to print the memory layout of this process, see if it matches with
// /proc/<self>/maps perhaps

// we need to get the task_struct from the process description. It has a field
// for mm, containing the mm_struct. The mm_struct, finally, contains all the addresses
// where the various memory segments start and where most end

int main(int argc, char **argv) {
  // yep the output matches with /proc/<pid>/maps [heap] section!
  printf ("heap offset(?):%x\n", sbrk (0));
  return 2;
}

long foo (long x) {
  while(1) {
    
    
  };
  
}

void waitForInput (){
  int i;
  scanf ("%d", &i);
}
