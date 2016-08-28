#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/user.h> // for: struct user_regs_struct !

#include "util.h"


// these are highly platform dependent, since those are the registers of the machine. For
// example on my x86_64 the struct is of the form:

/* struct user_regs_struct */
/* { */
/*   ...
/*   __extension__ unsigned long long int rax; */
/*   __extension__ unsigned long long int rcx; */
/*   __extension__ unsigned long long int rdx; */
/*   __extension__ unsigned long long int rsi; */
/*   __extension__ unsigned long long int rdi; */
/*   __extension__ unsigned long long int orig_rax; */
//   ...
/* }; */
// The full implementation can be found on /usr/include/sys/user.h:
// It could vary on your machine use gcc -H -fsyntax-only <program.c> to get
// a list of the header sources included in given program


void poke_user(pid_t tracee_process, int word_offset, long long int word) {
  // Copy the word _data_ to offset _addr_
  printf("    inside poke_user, word_offset: %d, word %llu\n", word_offset, word);
  //                                          *addr       *data
  ptrace(PTRACE_POKEUSER, tracee_process, 8 * word_offset, word);
}

void poke_user_interactively(pid_t tracee_pid) {
  int word_offset;
  long long int word;
  printf("write to user word index: ");
  scanf("%d", &word_offset);
  printf("hex: ");
  scanf("%llx", &word);
  poke_user(tracee_pid, word_offset, word);
}


void print_peek_data(pid_t tracee_pid, int byte_offset) {
  long long int peek_output;
  peek_output = ptrace(PTRACE_PEEKDATA, tracee_pid, byte_offset, NULL);
  // if ptrace() is unsuccessful it returns -1 and "errno" is set. "errno" is a global
  // variable (errno.h) that is set whenever a syscall causes a mistake. Finally strerror()
  // returns the string describing the errno error-number
  if (peek_output = -1) {
    printf("errno: %s\n", strerror(errno));
  }
  printf("PEEKDATA: %lx\n", ptrace(PTRACE_PEEKDATA, tracee_pid, byte_offset, NULL));


}

void print_peek_data_interactively(pid_t tracee_pid) {
  int input;
  printf("peekdata hexaddr: ");
  scanf("%x", &input);
  print_peek_data(tracee_pid, input);
}

void print_peek_data_at_rip(pid_t tracee_pid) {
  
}

/**
 * This assumes that the tracee_pid referenced process is already traced!
 * Interactively queries what reagion of the USER area of the
 * process to print.
 */
void print_peek_user_interactively(pid_t tracee_pid) {
  int input;
  printf("read from user word index: ");
  scanf("%d", &input);
  // the layout of user_regs and how we PEEK into it must not necessarily align, according
  // to the documentation. But it seems to work on x86_64+linux

  // NOTE: if PTRACE_PEEK* calls are unsuccessful they return zero!

  // on offset word alignment:
  // PEEKUSER will return a "word" on 64bit architecture Linux it is 64bit (long int)
  // A word is then compromised of 8 bytes (byte = 8 bit, 64 / 8 = 8). This is important
  // to know because the addr* argument given to ptrace() in this case is the offset,
  // well and offsets deal in bytes. So to read meaningful words we must pass it an offset
  // that is correctly aligned. That's why we multiply the input by the magic number 8
  print_peek_user(tracee_pid, input);
}

void print_peek_user(pid_t tracee_pid, int word_offset) {
  printf("PEEKUSER: %lx\n", ptrace(PTRACE_PEEKUSER, tracee_pid, 8 * word_offset, NULL));
}



// only works on x86_64 architecture, as the fields differ of the struct!
void print_user_regs_struct(struct user_regs_struct regs) {
  // %x is hex representation with lowercase letters - easier to read
  // (!) the description on the right is the for the System V (e.g. Linux) x86_64 calling
  // convention ! (!)
  printf ("r15:     %16llx %30s\n", regs.r15, "general purpose registers");
  printf ("r14:     %16llx\n", regs.r14);
  printf ("r13:     %16llx\n", regs.r13);
  printf ("r12:     %16llx\n", regs.r12);
  printf ("rbp:     %16llx\n", regs.rbp);
  printf ("rbx:     %16llx\n", regs.rbx);
  printf ("r11:     %16llx\n", regs.r11);
  printf ("r10:     %16llx\n", regs.r10);
  printf ("r9:      %16llx   %s\n", regs.r9, "6.");
  printf ("r8:      %16llx   %s\n", regs.r8, "5.");
  printf ("rax:     %16llx\n", regs.rax);
  printf ("rcx:     %16llx   %s\n", regs.rcx, "4.");
  printf ("rdx:     %16llx   %s\n", regs.rdx, "3.");
  printf ("rsi:     %16llx   %s\n", regs.rsi, "2.");
  printf ("rdi:     %16llx %30s\n", regs.rdi, "1. function/syscall argument"); // aka "parameter registers"
  printf ("orig_rax:%16llx\n", regs.orig_rax);
  printf ("rip:     %16llx %30s\n", regs.rip, "instruction pointer");
  printf ("cs:      %16llx\n", regs.cs);
  printf ("eflags:  %16llx\n", regs.eflags);
  printf ("rsp:     %16llx %30s\n", regs.rsp, "  Stack Pointer (current location in stack)");
  printf ("ss:      %16llx\n", regs.ss);
  printf ("fs_base: %16llx\n", regs.fs_base);
  printf ("gs_base: %16llx\n", regs.gs_base);
  printf ("ds:      %16llx\n", regs.ds);
  printf ("es:      %16llx\n", regs.es);
  printf ("fs:      %16llx\n", regs.fs);
  printf ("gs:      %16llx\n", regs.gs);
}


void print_endianness() {
  // testing endianness:
  // Endianness is = _byte_ order !!!
  // 1. &x return the adress of the int
  // 2. (char*) &x cast it to as a char pointer
  // 3. ((char*) &x)[0] read the first char (there are 4 chars in an int as one char = 1
  //    byte, int = 4 bytes
  // 4. ((int)((char*) &x)[0]) interpret that first char as an int. Now if we have little-endian
  //    then the int x = 1 will be stored on the machine with least significant byte first ergo
  //    the "int 1" will look like this byte-1: 00000001 byte-2,3,4: 00000000  on little-endian
  //    on big-endian.
  // This is especially important to note when interpreting data from hex-editors
  int x = 1;
  printf("Machine uses ");
  if (1 == ((int)((char*) &x)[0])) { 
    printf("LITTLE-Endian\n");
  } else {
    printf("BIG-Endian\n");
  };
}
