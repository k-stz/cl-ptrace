#include <stdio.h>

int main(int argc, char **argv) {
  printf("%d\n", abs(-2)); 
  return 2;
}

int returnsTwo() {
  return 2;
}

void passByReference(int *x) {
  *x = 2;
}
