#include <stdio.h>
#include <inttypes.h>
#include <Zydis/Zydis.h>
int main()
{
    uint8_t data[] =
    {
        0x51, 0x8D, 0x45, 0xFF, 0x50, 0xFF, 0x75, 0x0C, 0xFF, 0x75,
        0x08, 0xFF, 0x15, 0xA0, 0xA5, 0x48, 0x76, 0x85, 0xC0, 0x0F,
        0x88, 0xFC, 0xDA, 0x02, 0x00
    };
    // Initialize decoder context.
    ZydisDecoder decoder;
    ZydisDecoderInit(
        &decoder,
        ZYDIS_MACHINE_MODE_LONG_64,
        ZYDIS_ADDRESS_WIDTH_64);
    // Initialize formatter. Only required when you actually plan to
    // do instruction formatting ("disassembling"), like we do here.
    ZydisFormatter formatter;
    ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
    // Loop over the instructions in our buffer.
    // The IP is chosen arbitrary here in order to better visualize
    // relative addressing.
    uint64_t instructionPointer = 0x007FFFFFFF400000;
    size_t offset = 0;
    size_t length = sizeof(data);
    ZydisDecodedInstruction instruction;
    while (ZYDIS_SUCCESS(ZydisDecoderDecodeBuffer(
        &decoder, data + offset, length - offset,
        instructionPointer, &instruction)))
    {
        // Print current instruction pointer.
        printf("%016" PRIX64 "  ", instructionPointer);
        // Format & print the binary instruction
        // structure to human readable format.
        char buffer[256];
        ZydisFormatterFormatInstruction(
            &formatter, &instruction, buffer, sizeof(buffer));
        puts(buffer);
        offset += instruction.length;
        instructionPointer += instruction.length;
    }
}
