
#include <stdio.h>

char string[] = "Find this String in memory";

int x = 0;
int flagg = 0xabcd;

int main(int argc, char **argv) {
  /* flagg will be found in the RAX register! */
  while(flagg == 0xABCD) {
    x++;
  }
  printf("Left the endless loop, because flagg = %d!\n", flagg);
  printf("Secret string is: %s\n", string);
  return x;
}
