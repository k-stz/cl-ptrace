/* print some spam - used to test singlestepping for example */

#include <stdio.h>

char string[] = "Find this String in memory";

int x = 0;
int flagg = 43981; /* Hex representation: abcd */

int main(int argc, char **argv) {
  /* flagg will be found in the RAX register! */
  while(flagg == 43981) {
    x++;
  }
  return x;
}
