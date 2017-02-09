(asdf:load-system :cffi)

(defpackage :cffi-user
  (:use :common-lisp :cffi))

(in-package :cffi-user)

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


;; following the cffi tutorial for now and testing libcurl

;; (defcfun "ptrace" int ((__ptrace_request :enum)))

;; testing with self made libraries:
(define-foreign-library libtest
  ;; none of these work..
    (t (:default "/home/k-stz/sol_sanctum/cl-ptrace/bin/libtest")))

;; nope only works on *.so files (shared object files) !
;; tricking it into loading it anyway raises the signal: "(...) cannot dynamically load
;; executable"
(use-foreign-library libtest)

(defcfun "returnsTwo" :int)

(defcfun "foo" :long (x :long))

(defctype pid-t :long)


;; long int ptrace(enum __ptrace_request request, pid_t pid,
;;                 void *addr, void *data)

;; sao location: /usr/include/x86_64-linux-gnu/sys/ptrace.h

;; error occured?

;; ptrace returning -1 might indicate that an error, has happened (or the return value is
;; indeed -1). This is when we check *errno* which will be '0' on "Success". But due to
;; cffi we have the problem that the return value of ptrace gets translated to Lisp. -1 is
;; represented as #xffffffffffffffff, so we instead will test against that return value. Tests
;; indicated that this is semantically correct
;; The problem araised from using the ptrace return type :unsigned-long-long instead of :long-long
;; which would indeed return -1, but bogus for some other values read with peekdata.
(defcfun "ptrace" :unsigned-long-long
  ;;here the multiple arguments follow:
  (ptrace-request :int) (pid pid-t) (addr :pointer) (data :pointer))

(defcfun "waitpid" :int (pid-t :int) (status :pointer) (options :int))

;; TODO where are they implemented, also in a .h file?
(defconstant +SIGCONT+ 18)
(defconstant +SIGSTOP+ 19)

(defcfun "kill" :int (pid-t :int) (signal :int))

;; "Indicate that the process making this request should be traced.
;; All signals received by this process can be intercepted by its
;; parent, and its parent can use the other `ptrace' requests."
(defconstant +PTRACE-traceme+ 0)
;; returs the _word_ in the process's text/data/user space at address ADDR (2nd argument to ptrace)
;; the word size
;; Remember: the `word' size is architecture dependent! On x86_64 it is 64bits!
;;           so you ought to provide it ptrace a 8 byte container!
;; "Linux does not have seprate text and data address spaces".
;; peektext and peekdata are equivalent request on linux!!
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


(defconstant +ptrace-seize+ #x4205
  "Like PTRACE_ATTACH, but do not force tracee to trap and do not affect signal or group stop state.")

(defconstant +PTRACE-detach+ 17)

(defconstant +PTRACE-syscall+ 24)



(defvar null-value (null-pointer))
(defconstant +NULL+ null-value)


(defvar *pid* 0 "The process id that functions refer to when non specified") ;; 


;; WORKS, when Lisp is run as root!!!
(defun attach-to (&optional (pid *pid*))
  (let ((status (foreign-alloc :int)))
    (print "ptrace ptrace-attach..")
    (ptrace +PTRACE-ATTACH+ pid +NULL+ +NULL+)
    (print "waitpid..")
    (waitpid pid status 0)
    (format t "waitpid status: ~a~%" (mem-ref status :int))
    (format t "attached to process PID: ~a ~%" pid)
    (foreign-free status)) ;; TODO replace with (with-foreign-object ...) or put in *variable*
  pid)

;; add some datastructure to capture the state of a process, such as if it is already
;; traced, or ptrace returns a signal accordingly somehow if we try to detach from an
;; non-traced process?
(defun detach-from (&optional (pid *pid*))
  (ptrace +ptrace-detach+ pid +null+ +null+))


;; testing pass-by-reference
(defcfun ("passByReference" pbr) :void (x :pointer))

(defun am-i-root? ()
  (= (sb-posix:getuid) 0))

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


;; TODO extend SETF to replace this
(defun set-user-register (user-regs-struct register new-value)
  (setf  (foreign-slot-value user-regs-struct '(:struct user-regs-struct) register)
	 new-value))


(defun setregs (regs &optional (pid *pid*))
  (ptrace +ptrace-setregs+ pid +null+ regs))

(defun print-user-regs-struct (regs &optional (show-description-p t))
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
			  ;; the most useful flaggs see notes.org
			  (eflags "flags used for results of operations and cpu control")
			  (rsp "Stack Pointer to last item pushed on stack; grows to lower addresses")
			  (ss)
			  (fs_base)
			  (gs_base)
			  (ds)
			  (es)
			  (fs)     
			  (gs))
     :do
     ;; ~( x ~) <- downcase hex numbers directive!, #xFF -> #xff
       (format t "~8a:~(~20x~)   ~a~%" 
	       (car register)
	       (foreign-slot-value regs '(:struct user-regs-struct) (car register))
	       (if (or (null (second register)) (not show-description-p))
		   #\Space 
		   (second register)))))

(defun print-user-regs-struct-from-pid (&optional (pid *pid*))
  (with-foreign-object (regs '(:struct user-regs-struct))
    (getregs pid regs) ;; this implicitly sets `regs', which is bad lisp style (TODO)!
    (print-user-regs-struct regs)))

(defun hex-print (number &optional (destination t))
  (format destination "~(~x~)~%" number))

(defun rip-address (&optional (pid *pid*))
  ;; btw (let ((regs *regs*)) ) doesn't work, modifying `regs' will modify `*regs*'
  ;; beyond the lexical scope of `regs'
  (with-foreign-object (regs '(:struct user-regs-struct))
    (getregs pid regs)
    (foreign-slot-value regs '(:struct user-regs-struct) 'rip)))

(defun singlestep (&optional (pid *pid*) (print-instruction-pointer? t))
  (ptrace +ptrace-singlestep+ pid +null+ +null+)
  (waitpid pid +null+ 0)
  (when print-instruction-pointer?
      (with-foreign-object (regs '(:struct user-regs-struct))
	(getregs pid regs) ;; regs gets set here pass-by-reference style
	(format t "rip: ~x" 
		(foreign-slot-value regs '(:struct user-regs-struct) 'rip)))))

(defun allocate-user-regs ()
  (foreign-alloc '(:struct user-regs-struct)))

(defvar *regs* (allocate-user-regs)) ;; don't run multiple times!

(defun step-loop (&optional (pid *pid*))
  (loop 
     :with input :do
     (print-user-regs-struct-from-pid pid)
     (format t "(s)tep (q)uit~%")
     (setf input (read))
     (case input
       (s (singlestep pid))
       (q (return) ;; return from this loop
	  )
       (t ;; but nobody came
	))))




;; TODO: (1) tracee will stop whenever a signal is delivered. Test this, and how to
;;       get at the signal. (2) the tracer will be notified at its next call to waitpid(2)
;;       the &status variable of the waitpid will contain information that indicate the cause of the
;;       stop of the tracee.

(defcvar "errno" :int)
(get-var-pointer '*errno*)

(defcfun ("strerror" strerror-arg) :string (errno :int))

(defun strerror ()
  "Return String describing the *errno* number."
  (strerror-arg *errno*))

(defun peekdata (byte-offset &optional (pid *pid*) (print-errno-description? t) (hex-print? t))
  (let ((peeked-data))
    (setf peeked-data (ptrace +ptrace-peekdata+ pid (make-pointer byte-offset) +null+))
    (ptrace-successful? peeked-data print-errno-description?)
    (when hex-print?
      (hex-print peeked-data))
    peeked-data))

(defun pokedata (byte-offset data &optional (pid *pid*) (print-errno-description? t))
  (let ((ptrace-return-value))
    (setf ptrace-return-value
	  (ptrace +ptrace-pokedata+ pid (make-pointer byte-offset) (make-pointer data)))
    (ptrace-successful? ptrace-return-value print-errno-description?)))

(defun ptrace-successful? (ptrace-return-value &optional (print-errno-description? t))
  "Return T if last ptrace call was successful. Optionally print human readable errno description."
  (if (and (= ptrace-return-value #xffffffffffffffff) (/= *errno* 0))
      (progn
	(when print-errno-description?
	  (format t "~a~%" (strerror)))
	(values nil ptrace-return-value))
      (values t ptrace-return-value)))

#+sbcl
(defun endianess ()
  (if (find :little-endian *features*)
      :little-endian
      :big-endian))

;; on #+sbcl (machine-type) will return the architecture!
;; (machine-type) => "X86-6 4"

(defun find-instruction-loop ()
  (let  ((anchor-rip (rip-address))
	 (i -1)) 
    (singlestep)
    (loop while (/= anchor-rip i) do
         (setf i (rip-address))
	 (format t "~x~%" i)
	 (singlestep *pid* nil))))


;; well this is useless, it corresponds with /proc/<pid>/maps but just
;; for the first two address ranges. 
(defun find-readable-memory (from-num to-num &optional (pid *pid*))
  (let ((first-readable nil)
	(last-readable from-num)
	(start-set? nil))
    (loop for address from from-num to to-num  
       for peeked-data = (peekdata address pid nil nil) do
	 (if (ptrace-successful? peeked-data nil)
	     ;; success
	     (when (not start-set?)
	       (setf first-readable address) 
	       (setf start-set? t))
	     ;; not readable
	     (when start-set?
	       (setf start-set? nil)
	       (setf last-readable address)
	       (format t "readable: ~(~x~) - ~(~x~)~%" first-readable last-readable))))
    ;; left the loop, now either no readable found, or no unreadable found yet
    (when start-set?
      (format t "readable: ~(~x~) - ~(~x~)~%" first-readable to-num))))


(defun print-peekdata-over (n)
  (loop for i upto n
     for rip = (rip-address) do
       (format t "rip: ~x ~x~%"
	       rip
	       (peekdata rip *pid* nil))
       (singlestep *pid* nil)))


(defun get-maps-path (pid)
  (concatenate 'string
	       "/proc/"
	       (format nil "~a" pid)
	       "/maps"))


(defun parse-proc-pid-maps (pid)
  "Return list of Strings containing the line entries of /proc/<pid>/maps"
  (with-open-file (maps-stream (get-maps-path pid) :direction :input)
    ;; condition of type END-OF-FILE
    (loop for text = (read-line maps-stream nil nil) 
       while text ;; nil
	 collect text)))


;; TODO continue implementation
;; (defun get-address-range (maps-string-lines &key (memory-segment :data-segment))
;;   "Memory Segment can be either explicit [heap], [stack], [vdso], etc or. :data-segment"
;;   (lisp map-string-lines))

;; (defvar *maps-string-lines* nil)


(defun find-match-address-full (value from-address to-address &optional (pid *pid*))
  (loop for address from from-address to to-address
     :when (= value (peekdata address pid nil nil))
       collect address))


(defun find-match-address-partial (value from-address to-address &optional (pid *pid*))
  (loop for address from from-address to to-address
     :when (ends-with-bytes? (peekdata address pid nil nil)
			     value)
       collect address))


;; TODO: (bit-vector -1) = (integer->bit-vector 3)
;; check if this will be an issue
(defun integer->bit-vector (n &optional (size 64))
  "Convert integer to bit-vector representation of fixed `size'. Only works
with _positive_ integers"
  (let* ((bit-string (format nil "~b" n))
	 (bit-length (length bit-string))
	 (prepend-zeros
	  (progn
	    ;; if this assert signals, the bit-vector representation of n is bigger than
	    ;; the size of the target bit-vector we want to create
	    (assert (>= size bit-length))
	    (make-string (- size bit-length) :initial-element #\0)))
	 (full-bit-string (concatenate 'string prepend-zeros bit-string))
	 (full-bit-list
	  (loop for char in (coerce full-bit-string 'list) 
	     :if (char= char #\0)
	     collect 0
	     :else
	     collect 1)))
    (make-array size
		:element-type 'bit
		:initial-contents full-bit-list)))

;; implementation idea from edgar-rft
(defun bit-vector->integer (bit-vector)
  "Convert bit-vector to _positive_ integer."
  (reduce #'(lambda (first-bit second-bit)
	      (+ (* first-bit 2) second-bit))
	  bit-vector))

(defun bit-mask-padding (number &optional (size 64))
  (let* ((bit-string (format nil "~b" number))
	 (bit-length (length bit-string))
	 (mask-zeroes
	  (make-array (- size bit-length) :element-type 'bit :initial-element 0))
	 (mask-ones
	  (make-array bit-length :element-type 'bit :initial-element 1)))
    (concatenate 'bit-vector mask-zeroes mask-ones)))

(defun bit-mask (number &optional (size 64))
  "Return bit-vector bit-mask for `number' that can be used to clear bitvectors to insert
  `number' in them as bit-vector representations.  Example:
   (bit-mask 2 8)            ==> #*11111100
   (integer->bit-vector 2 8) ==> #*00000010 "
  (bit-not (bit-mask-padding number size)))


(defun ends-with-bytes? (target-number match-number)
"Returns true if `target-number' ends the`match-number' when, where both will be compared as 
byte their byte representation.

For example (ends-with-bytes ==> #x400500 #x500 true), even though #x400500 = 4195584 \=
#x500 = 1280"
  (assert (and (<= target-number (expt 2 64))
               (<= match-number (expt 2 64))))
  (let* ((bit-v1 (integer->bit-vector target-number))
	 (match-bytes-bit-vector (integer->bit-vector match-number))
	 (match-bytes-mask (bit-mask-padding match-number))
	 (masked-match-num (bit-and bit-v1 match-bytes-mask)))
    (equal masked-match-num
	   match-bytes-bit-vector)))

;; TODO rename
(defun pokedata-input-only (byte-offset data &optional (pid *pid*) (print-errno-description? t))
  "Only `pokedata' the bytes given in `data' at the address `byte-offset'. i.e. data = #xabcd,
will only replace the first 2 bytes with #xab #xcd instead of the hole 64-bit double word,
at address `byte-offset'"
  (let* ((peeked-bit-vector (integer->bit-vector (peekdata byte-offset pid nil nil)))
	(data-bit-vector (integer->bit-vector data))
	(data-mask (bit-mask data))
	 result-pokedata)
    ;; clear from target vector's place to fit in `data', using data-mask
    ;; then bit in `data' with bit-ior. Finally convert to an integer,
    ;; ready for being `pokedata'd
    (setf result-pokedata
	  (bit-vector->integer
	   (bit-ior (bit-and peeked-bit-vector data-mask)
		    data-bit-vector)))
    (pokedata byte-offset result-pokedata pid print-errno-description?)))
