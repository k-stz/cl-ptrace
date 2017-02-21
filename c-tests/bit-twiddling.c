#include <stdio.h>

int main(int argc, char **argv) {
  /* replacebyte(int index, int value byte replace byte) {} */
  /* shiftbits = 8 * index */
  /* mask = ~(0xff << shiftbits); */
  /* return value & mask | (replaceByte << shiftbits); */
  ///           ^^^^ clears out, replacebyte position
  //                   ^^^^ overwrite with replacebyte, which has been
  //                        lined up with the cleared out place with shiftbits
  // api: replacebyte(0, 0xffffffff, =x0a) => fffff0a

  int shiftbits = 8 * 3; 
  int mask = ~(0xff << 8);
  int x =  0x0a << 8 * 2;
  printf("shiftbits: %x \n mask: %x \n main: %x \n", shiftbits, mask, x);
  return 0;
}
