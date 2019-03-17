#include <stdio.h>
#include <inttypes.h>
#include <Zydis/Zydis.h>
#include <Zycore/LibC.h>


//Test x;
#include <stdint.h>
uint64_t x = 22;
/* From Formater01.c: */
/* #include <inttypes.h> */
/* #include <Zycore/Format.h> */
/* #include <Zycore/LibC.h> */
/* #include <Zydis/Zydis.h> */

// everything got installed into /usr/local/*

// zydis already includes: Zycore/Defines.h, Zycore/Types.h
/* #include <Zydis/Zydis.h>  //<- /usr/local/include/Zydis/Zydis.h */
/* #include "/usr/local/include/Zydis/Zydis.h" */
/* #include <Zycore/Format.h> */
/* #include <Zycore/LibC.h> */
/* #include <Zydis/Decoder.h> */

/* #include <Zydis/Decoder.h> */
//#include "zydis/include/Zydis/Zydis.h"

/* #include <Zycore/Zycore.h> */
/* #include <Zycore/LibC.h> */

//#include "/usr/local/include/Zydis/Zydis.h"
// ^^^^^^^^^^^^^^^^ is equivalent to the following: (!)


//void ZydisGetVersion() {}
// Definition:
// ZyanU64 zydis/dependencies/zycore/include/Zycore/Types.h:
//    typedef uint64_t  ZyanU64;
// uint64_t is a unsinged integer of exactly 64bits



int main() {
  ZydisGetVersion(); // is defined as,
  // in the file: "zydis/include/Zydis/Zydis.h"
  //  ZYDIS_EXPORT ZyanU64 ZydisGetVersion(void);

  // typedef ZyanU64 ZydisInstructionAttributes;
  /* ZyanU64 x = 2; */
  printf("x: %d\n", (int) x);
  printf("Zydis_version: %d\n", (int) ZydisGetVersion());

  if (ZydisGetVersion() != ZYDIS_VERSION)
    {
        fputs("Invalid zydis version\n", ZYAN_STDERR);
        return EXIT_FAILURE;
    }
    /* ZyanU8 data[] = */
    /* { */
    /*     0x51, 0x8D, 0x45, 0xFF, 0x50, 0xFF, 0x75, 0x0C, 0xFF, 0x75, */
    /*     0x08, 0xFF, 0x15, 0xA0, 0xA5, 0x48, 0x76, 0x85, 0xC0, 0x0F, */
    /*     0x88, 0xFC, 0xDA, 0x02, 0x00 */
    /* }; */

    /* // Initialize decoder context */
    /* ZydisDecoder decoder; */
    /* ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_ADDRESS_WIDTH_64); */
   
}
