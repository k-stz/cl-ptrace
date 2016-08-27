// including header files,
// is sematically the same as posting the contents of its belonging file here. #include is
// a preprocessor directive that takes care of that for us, it searches for a the given
// argument header in a standard list of system directories.  You can add directories to
// this list with the -I option while compiling the source code
// On my machine the files can be found in /usr/include for example sys/user.h is in
// /usr/include/sys/user.h

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


// ptrace prototype/signature:
// #include <sys/ptrace.h>
// long int ptrace(enum __ptrace_request request, pid_t pid,
//                 void *addr, void *data)


pid_t target_process;

char input_char;

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

// weird, note how we need to put "struct" before the signatures formal parameters or else
// the compiler complains..
// TODO this prototype is wrong? foo(type), instead of foo(type x)
void print_user_regs_struct(struct user_regs_struct regs); 

void poke_user(pid_t target_process, int word_offset, long long int word) {
  // Copy the word _data_ to offset _addr_
  printf("    inside poke_user, word_offset: %d, word %llu\n", word_offset, word);
  //                                          *addr       *data
  ptrace(PTRACE_POKEUSER, target_process, 8 * word_offset, word);
}

void poke_user_interactively(pid_t target_process) {
  int word_offset;
  long long int word;
  printf("write to user word index: ");
  scanf("%d", &word_offset);
  printf("hex: ");
  scanf("%llx", &word);
  poke_user(target_process, word_offset, word);
}

void print_peek_user(pid_t target_process, int word_offset) {
  printf("PEEKUSER: %llx\n", ptrace(PTRACE_PEEKUSER, target_process, 8 * word_offset, NULL));
}

// TODO: the following is used to test if the user area only compromises of the 
//       user_regs struct, and if peek_user is a pinpoint version of get_regs
/**
 * This assumes that the target_process is already traced!
 * Interactively queries what reagion of the USER area of the
 * process to print.
 */
void print_peek_user_interactively(pid_t target_process) {
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
  print_peek_user(target_process, input);
}

struct user_regs_struct regs;

int status;

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: ./attach-to.c <pid-of-target-process>\n");
  } else {
    target_process = atoi(argv[1]);
    printf("REACHED pid:%d \n", target_process);
    // process will be stopped
    ptrace(PTRACE_ATTACH, target_process, NULL, NULL);
    // according to the man page the waitpid ensures that we proceed only with the process
    // stopped:
    waitpid(target_process, &status, 0);
    printf("PTRACE_ATTACH waitpid status: %d \n", status);

    printf("now lets step through the child\n");
    printf("Press ENTER to step or q and then ENTER to quit\n");
    while( input_char != 'q') {
      printf("while() entered\n");
      ptrace(PTRACE_GETREGS, target_process, NULL, &regs);
      

      ptrace(PTRACE_SINGLESTEP, target_process, NULL, NULL);
      waitpid(target_process, &status, 0);
      // thanks to waitpid the code here now definitely deals with a stopped child at its
      // next instruction:
      poke_user_interactively(target_process);
      print_user_regs_struct (regs);
      print_peek_user_interactively(target_process);

      input_char = getchar();
    }
    printf("singlestepping aborted.\n");
    ptrace(PTRACE_DETACH, target_process, NULL, NULL);

  };

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
