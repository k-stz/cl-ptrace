/* Header files hold the definition and declaration shared among files that #include each
   other.
   It should only contain 
   - function prototype (also called function declarations)
   - variable declarations
   - global constants (#define const)
   - own type definitions (with typedef struct, union, enum)
   
   #include "file" will search for "file" in the same directory
   #include <file> will search in implementation dependent locations (mandated by the compiler/IDE)
            this is usually used for standard libary header files. On my machine for example they
	    can be found in /usr/include/* _among others_
 */


// "Include guards"
// ifndef is a reader-macro that tests if "symbol" given is not defined yet and only then
// will the rest of the block be passed to the compiler. The block ends at a #endif it
// if it was not defined yet #ifndef UTILS
/* #define UTILS */
// They're used so that files  might need other headers don't have to load their definitions again
// if they're already known by the overall program.

#include <stdbool.h>

void print_user_regs_struct(struct user_regs_struct);

// User area operation
void print_peek_user(pid_t tracee_pid, long long int word_offset);

void print_peek_user_interactively(pid_t tracee_pid);

void poke_user(pid_t tracee_pid, long long int word_offset, long long int word);

void poke_user_interactively(pid_t tracee_pid);

// Data area operation

// will read from the byte_offset'th byte.. if that makes sense
// this way we can use the instruction pointer value directly (regs.rip)!
void print_peek_data(pid_t tracee_pid, long long int byte_offset);

void print_peek_data_interactively(pid_t tracee_pid);

// print the from the tracee data area the value the instruction pointer currently points
// to, ergo the next instruction to be executed when singlestepping!
void print_peek_data_at_rip(pid_t tracee_pid);


/*
  Interactively querries for input to perform all the above operations on the child.  It
  returns a boolean signaling if the function's loop has been terminated due to user
  wanting to 'q' quit (false), else true.

*/
// TODO: The function is not fully decoupled from the program flow as terminating the loop
// with input 's' from the user expects it to run till the next (s)tep.
bool peekpoke_interactively(pid_t tracee_pid ,struct user_regs_struct regs);
