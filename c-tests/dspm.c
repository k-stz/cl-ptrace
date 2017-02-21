/* print some spam - used to test singlestepping for example */

#include <stdio.h>
#include <time.h>
#include <sys/ptrace.h>


char string[] = "Find this String in memory";

int x = 0;
int flagg = 43981; /* Hex representation: abcd */

int main(int argc, char **argv) {
  int begin, end;
  begin = clock();
  int i;
  
  for ( i = 0; i < 200000000; i++) {
    ptrace(PTRACE_PEEKUSER, 0, i, 0);
  }
  end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("time_spent: %f\n", time_spent);
  /* printf("time_spent: %d \n", (double) clock() - ); */

  /* flagg will be found in the RAX register! */
  /* while(flagg == 43981) {  */
  /*   /\* TODO: it seems RAX alters between containing "flagg" and x. find a way to display *\/ */
  /*   /\* the instructions executed *\/ */
  /*   x--; */
  /* } */
  return x;
}
