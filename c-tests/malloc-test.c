#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h> // sbrk()


void *ptr;

void waitForInput ();

int main(int argc, char **argv) {
  // testing if [heap] exists before/after malloc use
  printf ("before malloc\n");
  waitForInput ();

  ptr =  malloc(4);

  printf ("after malloc\n");
  // yep, checking /proc/<pid-of-this-process>/maps will show up
  // a [heap] section only _after_ malloc has been used!!!
  waitForInput ();
  
  printf("pointer address %p\n", ptr);
  // ptr = (int *) 0x400500;
  *((int *) ptr) = 0xabcdef;
  waitForInput ();
  // now let's test if we can peekdata from heap.. we will simply
  // attach to the process at this point and use the pointer address printed above
  // and see if we find the letters #xabcdef !
  // aaaand it works!!
  // (peekdata <ptr>) ==> abcdef !!!!
  // HAHA if we (pokedata <ptr> #xrandom)
  // and continue the execution it will even change the value!!!
  // meaning we can manipulate the process's runtime data with ptrace! 
  printf("pointer address %p\n", ptr);
  // if we were to (peekdata #x400000)
  // the printf("pointer content...") below would match!
  // But if you were to write something in that area, which is
  // usually the Data segment (or rather text segment)
  // *((int *) ptr) = 1223002440;
  // you will get a segmentation fault!
  printf("pointer content %x\n", *( (int *) ptr));

  // yep the output matches with /proc/<pid>/maps [heap] section!
  printf ("heap offset(?):%p\n", (unsigned int *) sbrk (0));

  return 0;
}

long foo (long x) {
  while(1) {
    
    
  };
  
}

void waitForInput (){
  int i;
  scanf ("%d", &i);
}
