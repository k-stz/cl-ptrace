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

void print_user_regs_struct(struct user_regs_struct);

// User area operation
void print_peek_user(pid_t tracee_pid, int word_offset);

void print_peek_user_interactively(pid_t tracee_pid);

void poke_user(pid_t tracee_pid, int word_offset, long long int word);

void poke_user_interactively(pid_t tracee_pid);

// Data area operation

// will read from the byte_offset'th byte.. if that makes sense
// this way we can use the instruction pointer value directly (regs.rip)!
void print_peek_user(pid_t tracee_pid, int byte_offset);

void print_peek_user_interactively(pid_t tracee_pid);



