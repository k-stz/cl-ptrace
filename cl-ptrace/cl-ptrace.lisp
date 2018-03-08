;; TODO remove package redundancy once .asd file loading works
(asdf:load-system :cffi)

(defpackage :cl-ptrace
  (:use :common-lisp :cffi))

(in-package :cl-ptrace)

;; This describes how to load the library _into the Lisp image_ !
(define-foreign-library libc
  ;; none of these work..
  (:unix (:or ;"libc-2.19"
	  "libc.so.6"
	  "libc.so"
	  "libc-2.19.so"
	  "/lib/x86_64-linux-gnu/libc-2.19"
	  "/lib/x86_64-linux-gnu/libc.so.6"
	  "/usr/lib/x86_64-linux-gnu/libc.so"
	  "libc.so.6" "libc.so" "libc-2.24.so" "libc.a"
	  "/usr/lib/libc-2.24.so" "/usr/lib/libc-2.24"))
    (t (:default "/lib/x86_64-linux-gnu/libc-2.19"
	   "/usr/lib/libc.so")))

;; this effectively loads libc into the Lisp image, much like we load .lisp files
;; into the lisp image. "much like the linker does _when you start_ a C program"
(use-foreign-library libc) ;; after this point "libc" is loaded in the Lisp image!!!


(defctype pid-t :long)

;; long int ptrace(enum __ptrace_request request, pid_t pid,
;;                 void *addr, void *data)

;; sao location: /usr/include/x86_64-linux-gnu/sys/ptrace.h;; error occurred?
;; ptrace returning -1 might indicate that an error, has happened (or the return value is
;; indeed -1). This is when we check *errno* which will be '0' on "Success". But due to
;; cffi we have the problem that the return value of ptrace gets translated to Lisp. -1 is
;; represented as #xffffffffffffffff, so we instead will test against that return value. Tests
;; indicated that this is semantically correct
;; The problem arose from using the ptrace return type :unsigned-long instead of :long
;; which would indeed return -1, but bogus for some other values read with peekdata.
(defcfun "ptrace" :unsigned-long
  ;;here the multiple arguments follow:
  (ptrace-request :int) (pid pid-t) (addr :pointer) (data :pointer))


;; CAN'T MMAP proc/<pid>/mem files!!!
(defun mmap-file (file-path &optional (permission-string "r--p"))
  "Return SAP, system-area-pointer, to the newly mapped file in the process memory.
`permission-string' example: \"rw-p\""
  (let ((permission-logior (permission-string->posix-permission-logior permission-string))
	(flag (permission-string->flag-private/shared permission-string)))
    ;; mmap complains with flag 's' for SHARED if stream is just :input
    (with-open-file (stream file-path :direction :IO :if-exists :append)
      ;; (with-open-file (stream file-path :direction :input)  
      (let ((fd (sb-impl::fd-stream-fd stream))
	    (length (file-length stream))
	    (sap))
	(setf sap (osicat-posix:mmap +null+
				     length
				     permission-logior
				     flag
				     fd
				     0))
	sap))))

;; default to length #x1000 as that's the default virtual-memory-page-size on my system
(defun munmap (sap &optional (length #x1000))
  (osicat-posix:munmap sap length))



(defcfun "waitpid" :int (pid-t :int) (status :pointer) (options :int))

;; you can query the signal enum mapping with the shell command "kill -l" !
(defconstant +SIGKILL+ 9) ;; Exactly as in "kill -9 <pid>"
(defconstant +SIGCONT+ 18)
(defconstant +SIGSTOP+ 19 "Stop process")
(defconstant +SIGTSTP+ 20 "Stop typed at terminal[sic]")

;; Now we can also do a kill -9 <pid> simply with (kill <pid> 9)
(defcfun "kill" :int (pid-t :int) (signal :int))

;; "Indicate that the process making this request should be traced.
;; All signals received by this process can be intercepted by its
;; parent, and its parent can use the other `ptrace' requests."
(defconstant +PTRACE-traceme+ 0)
;; returns the _word_ in the process's text/data/user space at address ADDR (2nd argument to ptrace)
;; the word size
;; Remember: the `word' size is architecture dependent! On x86_64 it is 64bits!
;;           so you ought to provide it ptrace a 8 byte container!
;; "Linux does not have separate text and data address spaces".
;; peektext and peekdata are equivalent request on Linux!!
(defconstant +PTRACE-peektext+ 1)
(defconstant +PTRACE-peekdata+ 2) 
(defconstant +PTRACE-peekuser+ 3)

(defconstant +PTRACE-poketext+ 4)
(defconstant +PTRACE-pokedata+ 5)
(defconstant +PTRACE-pokeuser+ 6 )

(defconstant +PTRACE-cont+ 7 "Continue the process")
(defconstant +PTRACE-kill+ 8 "Kill the process")
(defconstant +PTRACE-singlestep+ 9)


(defconstant +PTRACE-getregs+ 12)
(defconstant +PTRACE-setregs+ 13)
;; ptrace_getfpregs 14
;; ptrace_setfpregs 15
(defconstant +PTRACE-attach+ 16)


(defconstant +ptrace-seize+ #x4206
  ;; peek/poke will only work though after the process has been SIGSTOP'ed!
  ;; afterwards SIGCONT won't work, you have to `DETACH-FROM' the process again!
  "Like PTRACE_ATTACH, but do not force tracee to trap and do not affect signal or group stop state.")

(defconstant +PTRACE-detach+ 17)

(defconstant +PTRACE-syscall+ 24)

;; because loading the .asd file, (defconstant ) is
;; doesn't see the value `null-value' which is defvar'd here.
;; so we make sure it is already available from whatever level
;; defconstant wants to have it
(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defvar null-value (null-pointer)))
(defconstant +NULL+ null-value)


(defvar *pid* 0 "The process id that functions refer to when non specified") ;; 

;; WORKS, when Lisp is run as root!!!
(defun attach-to (&optional (pid *pid*))
  (print "ptrace ptrace-attach..")
  (let ((ptrace-return-value
	 (ptrace +PTRACE-ATTACH+ pid +NULL+ +NULL+)))
    (if (ptrace-successful? ptrace-return-value)
	(with-foreign-object (status :int)
	  (print "waitpid..")
	  (waitpid pid status 0)
	  (format t "waitpid status: ~a~%" (mem-ref status :int))
	  (format t "attached to process PID: ~a ~%" pid)
	  pid)
	;; attaching failed?
	ptrace-return-value)))

(defun countdown (seconds)
  "Sleep for given `seconds' with textual feedback for each second 'counting down'. Can
be used to issue some commands after a certain amount of time, while handling input on another
Program."
  (loop for sec from seconds downto 1 do
       (format t "~a...~%" sec)
       (sleep 1))
  (format t "ACTION!"))


;; add some data structure to capture the state of a process, such as if it is already
;; traced, or ptrace returns a signal accordingly somehow if we try to detach from an
;; non-traced process?
(defun detach-from (&optional (pid *pid*))
  (let ((ptrace-return-value
	 (ptrace +ptrace-detach+ pid +null+ +null+)))
    (ptrace-successful? ptrace-return-value)
    ptrace-return-value))


;; from /usr/include/x86_64-linux-gnu/sys/ptrace.h (sao)
;; struct user_regs_struct  // x86_64 specific registers  !
;; {
;;   __extension__ unsigned long long int r15; 	
;;   __extension__ unsigned long long int r14;	   
;;   __extension__ unsigned long long int r13;	   
;;   __extension__ unsigned long long int r12;	   
;;   __extension__ unsigned long long int rbp;	   
;;   __extension__ unsigned long long int rbx;	   
;;   __extension__ unsigned long long int r11;	   
;;   __extension__ unsigned long long int r10;	   
;;   __extension__ unsigned long long int r9;	   
;;   __extension__ unsigned long long int r8;	   
;;   __extension__ unsigned long long int rax;	   
;;   __extension__ unsigned long long int rcx;	   
;;   __extension__ unsigned long long int rdx;	   
;;   __extension__ unsigned long long int rsi;	   
;;   __extension__ unsigned long long int rdi;	   
;;   __extension__ unsigned long long int orig_rax;
;;   __extension__ unsigned long long int rip;	   
;;   __extension__ unsigned long long int cs;	   
;;   __extension__ unsigned long long int eflags;  
;;   __extension__ unsigned long long int rsp;	   
;;   __extension__ unsigned long long int ss;	   
;;   __extension__ unsigned long long int fs_base; 
;;   __extension__ unsigned long long int gs_base; 
;;   __extension__ unsigned long long int ds;	   
;;   __extension__ unsigned long long int es;	   
;;   __extension__ unsigned long long int fs;	   
;;   __extension__ unsigned long long int gs;      
;; };
    
(defcstruct user-regs-struct
  (r15 :unsigned-long-long)
  (r14 :unsigned-long-long)
  (r13 :unsigned-long-long)
  (r12 :unsigned-long-long)
  (rbp :unsigned-long-long)
  (rbx :unsigned-long-long)
  (r11 :unsigned-long-long)
  (r10 :unsigned-long-long)
  (r9 :unsigned-long-long)
  (r8 :unsigned-long-long)
  (rax :unsigned-long-long)
  (rcx :unsigned-long-long)
  (rdx :unsigned-long-long)
  (rsi :unsigned-long-long)
  (rdi :unsigned-long-long)
  (orig_rax :unsigned-long-long)
  (rip :unsigned-long-long)
  (cs :unsigned-long-long)
  (eflags :unsigned-long-long)
  (rsp :unsigned-long-long)
  (ss :unsigned-long-long)
  (fs_base :unsigned-long-long)
  (gs_base :unsigned-long-long)
  (ds :unsigned-long-long)
  (es :unsigned-long-long)
  (fs :unsigned-long-long)
  (gs :unsigned-long-long))

;; c-struct allocation
;; (defparameter *regs* (foreign-alloc '(:struct user-regs-struct)))
;; (setf
;;  (foreign-slot-value *regs* '(:struct user-regs-struct) 'rax))
;; 42)

(defun getregs (pid regs)
  "Pass by reference, fills `regs' with the current register map of the traced process `pid'."
  (ptrace +ptrace-getregs+ pid +null+ regs)
  regs)
       				     
       				     
;; NEXT TODO: use with continue-singlestep; assoc list good idea? 
(defun regs-c-struct->lisp-regs (user-reg-struct)
  (let ((register-list '(r15 r14 r13 r12 rbp rbx r11 r10 r9 r8 rax rcx rdx rsi rdi
       			 orig_rax rip cs eflags rsp ss fs_base gs_base ds es fs gs)))
    (loop for register in register-list collect
	 (cons register
	       (foreign-slot-value user-reg-struct '(:struct user-regs-struct)
				   register)))))

;; You might want to extend SETF to replace this
(defun set-user-register (user-regs-struct register new-value)
  (setf  (foreign-slot-value user-regs-struct '(:struct user-regs-struct) register)
	 new-value))


(defun setregs (regs &optional (pid *pid*))
  (ptrace +ptrace-setregs+ pid +null+ regs))

(defun print-user-regs-struct (regs &optional (regs-filter-list nil)
				      (show-description-p t))
  "Use the `regs-filter-list' to only print the regs therein, the default
value `nil' means to print all regs."
                        ;;(<register> <optional description>)
  (loop for register in '((r15 "general purpose registers")
			  (r14)
			  (r13)
			  (r12)
			  (rbp)
			  (rbx)
			  (r11)
			  (r10)
			  (r9 "6.")
			  (r8 "5.")
			  (rax)
			  (rcx "4.; used for LOOPing in assembly..")
			  (rdx "3.")
			  (rsi "2.")
			  (rdi "1. function/syscall argument")
			  (orig_rax)
			  (rip "instruction pointer")
			  (cs)
			  ;; the most useful flags see notes.org
			  (eflags "flags used for results of operations and CPU control")
			  (rsp "Stack Pointer to last item pushed on stack; grows to lower addresses")
			  (ss)
			  (fs_base)
			  (gs_base)
			  (ds)
			  (es)
			  (fs)     
			  (gs))
     :do		   
     ;; ~( x ~) <- down case hex numbers directive!, #xFF -> #xff
       (if (or (null regs-filter-list) ;; if empty, don't filter any (default)
	       (find (car register) regs-filter-list))
	 (format t "~8a:~(~20x~)   ~a~%" 
		 (car register)
		 (foreign-slot-value regs '(:struct user-regs-struct) (car register))
		 (if (or (null (second register)) (not show-description-p))
		     #\Space 
		     (second register))))))

(defun print-user-regs-struct-from-pid (&optional regs-filter-list (pid *pid*))
  (with-foreign-object (regs '(:struct user-regs-struct))
    (getregs pid regs) ;; this implicitly sets `regs'
    (print-user-regs-struct regs regs-filter-list)))

(defun rip-address (&optional (pid *pid*))
  ;; btw (let ((regs *regs*)) ) doesn't work, modifying `regs' will modify `*regs*'
  ;; beyond the lexical scope of `regs'
  (with-foreign-object (regs '(:struct user-regs-struct))
    (getregs pid regs)
    (foreign-slot-value regs '(:struct user-regs-struct) 'rip)))

(defun singlestep (&optional (pid *pid*) (print-instruction-pointer? t))
  (ptrace +ptrace-singlestep+ pid +null+ +null+)
  (waitpid pid +null+ 0)
  (let ((rip-address (rip-address)))
    (when print-instruction-pointer?
      (with-foreign-object (regs '(:struct user-regs-struct))
	(getregs pid regs) ;; regs gets set here pass-by-reference style
	(format t "rip: ~x" 
		rip-address)
	(terpri)))
    rip-address))

(defun singlestep-peek-from-rip (n-instructions &optional (pid *pid*))
  (let* ((rip-peekdata (peekdata (rip-address) pid t nil))
	 (instructions-array (make-array n-instructions))
	 rip-address)
    (loop for nth-instruction from 0 below n-instructions do
	 (setf rip-address (rip-address pid)
	       rip-peekdata (peekdata rip-address pid nil nil))
	 (setf (aref instructions-array nth-instruction)
	       (list rip-address rip-peekdata))
	 (singlestep pid nil))
    instructions-array))

(defun allocate-user-regs ()
  ;; free with (foreign-free ..)
  (foreign-alloc '(:struct user-regs-struct)))

(defvar *regs* (allocate-user-regs)) ;; don't run multiple times!

(defun step-loop (&optional (regs-filter-list nil) (pid *pid*))
  (let ((print-regs t))
    (loop 
       :with input :do
       (print-user-regs-struct-from-pid regs-filter-list pid)
       (format t "peek rip:    ")
       (peekdata (rip-address))
       (format t "(s)tep (q)uit ~%")
       (setf input (read))
       (case input
	 (s (singlestep pid)
	    (setf print-regs t))
	 (i (peekdata (rip-address pid) pid t t))
	 (q (return) ;; return from this loop
	    )
	 (t ;; but nobody came
	  )))))

 
;; TODO: (1) tracee will stop whenever a signal is delivered. Test this, and how to
;;       get at the signal. (2) the tracer will be notified at its next call to waitpid(2)
;;       the &status variable of the waitpid will contain information that indicate the cause of the
;;       stop of the tracee.

(defcvar "errno" :int)
(declaim ((signed-byte 32) *errno*))
(get-var-pointer '*errno*)

(defcfun ("strerror" strerror-arg) :string (errno :int))

(defun strerror ()
  "Return String describing the *errno* number."
  (strerror-arg *errno*))

(defun ptrace-successful? (ptrace-return-value &optional (print-errno-description? t))
  "Return T if last ptrace call was successful. Optionally print human readable errno description."
  (declare ((unsigned-byte 64) ptrace-return-value))
  (if (and (= ptrace-return-value #xffffffffffffffff) (/= *errno* 0))
      (progn
	(when print-errno-description?
	  (format t "~a~%" (strerror)))
	(values nil ptrace-return-value))
      t))


(defun peekdata (byte-offset &optional (pid *pid*) (print-errno-description? t) (hex-print? t))
  (let ((peeked-data))
    ;; (make-pointer ...) isn't causing any consing..
    (setf peeked-data (ptrace +ptrace-peekdata+ pid (make-pointer byte-offset) +null+))
    (ptrace-successful? peeked-data print-errno-description?)
    (when hex-print?
      (hex-print peeked-data))
    peeked-data))

(defun pokedata-full-addr (byte-offset data &optional (pid *pid*) (print-errno-description? t))
  (let ((ptrace-return-value))
    (setf ptrace-return-value
	  (ptrace +ptrace-pokedata+ pid (make-pointer byte-offset) (make-pointer data)))
    (ptrace-successful? ptrace-return-value print-errno-description?)))


;; use this version from the repl
;; TODO, you cant write just '0' to an address!
(defun pokedata (byte-offset data &key (pid *pid*)
				    (print-errno-description? t)
				    (write-n-bits (integer-length data)))
  "Only `pokedata' the `write-n-bits' bits of `data' at the address `byte-offset'. i.e. data = #xabcd,
and default write-bits (integer-length data) = 16 Bits, will only replace the first 2
bytes, of the value at (pokedata byte-offset ..) with #xab #xcd instead of the whole
64-bit double word, at address `byte-offset'.

Remember ptrace(PTRACE_PEEKDATA,..) only allows to set full (64bit on x86_64) words at a
time, so this function takes care to only set the amount of bits you want."
  (let* ((peeked-data (peekdata byte-offset pid nil nil)))
    ;; this directly sets the bits in peeked-data to data
    ;; TODO: the default behaviour should be to always write the bits plus a padding leading
    ;; 0's so it always fills the last byte.
    ;; this is important as with the #x12 =binary> 10010 so that a write to a byte like
    ;; 11111111 would be 11110010, instead of just 00010010 (it must be very rare for a program
    ;; to read n-bits as data rather than the whole byte
    (format t "bits to write: ~a ~%" write-n-bits)
    (setf (ldb (byte write-n-bits 0) peeked-data) data)
    (pokedata-full-addr byte-offset peeked-data pid print-errno-description?)))


;;The idea was to find a game loop by running this multiple times:
(defun find-instruction-loop ()
  "Singlestep through the tracee and print the list
of addresses when encountering a loop."
  (let  ((anchor-rip (rip-address))
	 (i -1)) 
    (singlestep)
    (loop while (/= anchor-rip i) do
         (setf i (rip-address))
	 (format t "~x~%" i)
	 (singlestep *pid* nil))))


(defun print-n-peekdata-instructions (n &optional (pid *pid*))
  (format t "rip-address : peekdata-instruction~%")
  (loop for i below n
     for rip = (rip-address pid) do
       (format t "rip: ~x ~16,x~%"
	       rip
	       (peekdata rip pid nil nil))
       (singlestep pid nil)))


(defun ends-with-bits? (target-number match-number &optional (bits (integer-length match-number)))
  "Returns true if `target-number' numerical bits representation ends with `match-number's numerical 
bits representation. 

For example (ends-with-bytes ==> #x400500 #x500 true), even though #x400500 = 4195584 \=
1280 = #x500.

The `bits' argument can be used if only n-bits should be compared. By default all the bits needed
to represent `match-number' will be used.

The original purpose of this function is to help find a bit pattern in memory, by scanning through
it.
  (declare ((unsigned-byte 64) match-number target-number)
	   (fixnum bits))"
  (= (ldb (byte bits 0) target-number)
     (ldb (byte bits 0) match-number)))

(defun find-match-address-full (value from-address to-address &optional (pid *pid*))
  (loop for address from from-address to to-address
     :when (= value (peekdata address pid nil nil))
       collect address))


(defun find-value-address (value &key (pid *pid*)
				   (from-address #x0)
				   (to-address #x0)
				   (address-list nil)
				   (address-range nil)
				   (byte-padding t))
  "Returns a list of addresses for which (peekdata address ..) will return a number ending
with the bits representing `value'.

Will either search addresses :from-address to `:to-address', or a list of, for example
already filtered, addresses provided from `:address-list'"
  (let ((search-bits
	 (if byte-padding
	     (* 8 (integer-byte-length value))
	     (integer-length value))))
    (if (not (null address-list))
	(loop for address in address-list
	   :when (ends-with-bits? ;; (peekdata address pid nil nil)
		  (ptrace +ptrace-peekdata+ pid (make-pointer address) +null+)
		  value
		  search-bits)
	   collect address)
	(progn
	  (when address-range
	    (setf from-address (first address-range)
		  to-address (second address-range)))
	  (loop for address from from-address to to-address
	     :when (ends-with-bits? ;; (peekdata address pid nil nil)
		    (ptrace +ptrace-peekdata+ pid (make-pointer address) +null+)
		    value
		    search-bits)
	     collect address)))))

(defun find-nearby (value address &optional (search-distance 1000) (pid *pid*))
  "Search for `value' around the `address' by `search-distance' addresses.
This can be used search using the heuristic of related data being next to, or
nearby each other in memory."
  (let* ((from-address (- address search-distance))
	 (to-address (+ address search-distance))
	 (address-range
	  ;; doing some clamping to 0 and unsigned-byte 64 max size for
	  ;; the address range
	  (list (if (> 0 from-address) 0 from-address)
		(if (> to-address #xffffffffffffffff)
		    #xffffffffffffffff
		    to-address))))
    (find-value-address value :address-range address-range :pid pid)))


;; some test heuristic searches

(defun find-value-heuristic-1 (value address-range nearby-values-list &optional (search-distance 1000) (pid *pid*))
  "Returns a list where the first entry is the address where `value' was found
and the rest is a sum for each of the nearby-values found around that address.

For example: an entry like (7536124 2 1) indicates that `value' was found at address 7536124
and that first value in the `nearby-values-list' was found 2-times in the nearby and the second
value 1-time (length of the nearby-values-list is hence 2)."
  (let ((address-values-list
	 (find-value-address value :address-range address-range :pid pid)))
    (if (null address-values-list)
	(format t "the value ~a wasn't found in the address-range: ~a ~%" value address-range)
	(loop for suspect-value-address in address-values-list
	   :collect
	   ;; the inner loop returns a list where each element is a list
	   ;; of the found nearby-values
	     (cons suspect-value-address
		   (how-many-nearby-found
		    (list :suspect-value-address suspect-value-address
			  (loop for nearby-value in nearby-values-list
			     collect
			       (list :nearby-value nearby-value
				     (find-nearby nearby-value suspect-value-address search-distance))))))))))

;; uses the output of `find-value-heuristic-1'
(defun how-many-nearby-found (suspect-address-values-list)
  (let ((nearby-value-lists (caddr suspect-address-values-list)))
    (loop for entries in nearby-value-lists
       :collect
	 (length (third entries)))))

(defun found-one-of-each-heuristic-1 (value-heuristic-1-list)
  (loop for entry in value-heuristic-1-list
     :when (not (find 0 entry))
       :collect entry))


;; TEST, to mapcar over readable-address-ranges!
(defparameter heuristic-fn
  (lambda (address-range)
    (found-one-of-each-heuristic-1
     (find-value-heuristic-1 15000 address-range
			     (list 275 200) ; some example nearby-values
			     100 ; search-distance
			     ))))
(defparameter brute-force-fn
  (lambda (address-range)
    (find-value-address 204 :address-range address-range)))


