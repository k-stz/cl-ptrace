/* print some spam - used to test singlestepping for example */

#include <stdio.h>

// Found it! The characters are layout out, :little-endian,
// style on the machine. So "Find" is in memory "dniF" so to speak
// Also each char maps to good old ASCI, so we can use cl:char-int
// to querry the letters and finally each char needs just a byte
// to be represented. So find in memory is: 
// (reverse ;; because :little-endian
//     (mapcar #'char-int (coerce "Find" 'list))) */
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
