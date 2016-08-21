// print some spam - used to test singlestepping for example

#include <stdio.h>

char string[] = "Find this String in memory";

int main(int argc, char **argv) {
  while(1) {
    printf("Spam-.");
  }
  return 0;
}
