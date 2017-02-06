// task_struct->mm_struct fetching approach was appandoned. Apparently those are a no-no
// for userspace usage. Hence we will be parsing /proc/<pid>/maps instead (which are, btw,
// created by the kernel using those structs


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// to find the header for the usual systemcall functions use
// man 2 <function-name> !
#include <unistd.h> // sbrk(0)

#include <sched.h>



void waitForInput ();

// trying to print the memory layout of this process, see if it matches with
// /proc/<self>/maps perhaps

// we need to get the task_struct from the process description. It has a field
// for mm, containing the mm_struct. The mm_struct, finally, contains all the addresses
// where the various memory segments start and where most end


extern etext;
extern edata;
extern end;
extern foooo;

int main(int argc, char **argv) {
  // yep the output matches with /proc/<pid>/maps [heap] section!
  // these seem to match up somewhat with maps, but they take addresses between ranges..
  // also this is basically useless, as we want to query these of any process, not
  // from a process iteself, as that code we can't usually access without a lot of
  // involvement. Fetching task_struct/mm_struct shoul be the way to go
  printf ("heap offset(?):%p\n", sbrk (0));
  printf("main %p\n", main);
  printf("etext %p\n", &etext);
  printf("edata %p\n", &edata);
  printf("end %p\n", &end);
  waitForInput(); // here we can test the above matching with /proc/<pid>/maps
  
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
