#include <stdio.h>
#include <mm.h>

int main(int argc, char **argv) {
  printf("%d\n", abs(-2)); 
  return 2;
}

long foo (long x) {
  return x;
}

int returnsTwo() {
  return 2;
}

void passByReference(int *x) {
  *x = 2;
}
