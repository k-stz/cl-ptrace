(asdf:load-system :cffi)

(defpackage :cffi-user
  (:use :common-lisp :cffi))

(in-package :cffi-user)

;; example use
;; (define-foreign-library libcurl
;;     (:darwin (:or "libcurl.3.dylib" "libcurl.dylib"))
;;     (:unix (:or "libcurl.so.3" "libcurl.so"))
;;     (t (:default "/usr/lib/libcurl")))

;; (use-foreign-library libcurl)


;; This describes how to load the library _into the Lisp image_ !
;; hopefully libc-2.24 has and is what we need
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


;; this effectively loads libc into the Lisp image much, just like we load .lisp files
;; into the lisp image. "much like the linker does _when you start_ a C program
(use-foreign-library libc) ;; after this point "libc" is loaded in the Lisp image!!!


;; following the cffi tutorial for now and testing libcurl

;; (defcfun "ptrace" int ((__ptrace_request :enum)))

;;       
(defcfun ("abs" absoluto) :int (flags :int)) ; WOOOOOOOOOOOOOOOOOOOOOOOOOOORKS
;; "abs" is the name of the clib function
;; where `absoluto' refers to the name we can invoke it with in the lisp code!
;; :int return-value

;; testing with self made libraries:
(define-foreign-library libtest
  ;; none of these work..
    (t (:default "/home/k-stz/sol_sanctum/cl-ptrace/bin/libtest")))

;; nope only works on *.so files (shared object files) !
;; trickying it into loading it anyway raises the signal: "(...) cannot dynamically load
;; executable"
(use-foreign-library libtest) 

(defcfun "returnsTwo" :int)

(defctype pid-t :long)


 ;; long int ptrace(enum __ptrace_request request, pid_t pid,
 ;;                 void *addr, void *data)
                             
(defcfun "ptrace" (:long-long)
  ;;here the multiple arguments follow:
  (ptrace-request :int) (pid pid-t) (addr :pointer) (data :pointer))

(defcfun "waitpid" :int (pid-t :int) (status :pointer) (options :int))

(defconstant +PTRACE-singlestep+ 9)
(defconstant +PTRACE-getregs+ 12)
(defconstant +PTRACE-setregs+ 13)
(defconstant +PTRACE-attach+ 16)
(defconstant +PTRACE-detach+ 17)


(defvar null-value (null-pointer))
(defconstant +NULL+ null-value)


;; (ptrace +PTRACE-ATTACH+ <pid-t> +NULL+ +NULL+)
;; (waitpid <pid-t> status 0)
;; (ptrace +PTRACE-DETACH+ <pid-t> +NULL+ +NULL+)

;; WORKS, when Lisp is run as root!!!
(defun attach-to (pid)
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
(defun detach-from (pid)
  (ptrace +ptrace-detach+ pid +null+ +null+))

;; for quick testing
(defvar *pid*)
(defun attach ()
  (ptrace +ptrace-attach+ *pid* +null+ +null+))

(defun detach ()
  (ptrace +ptrace-detach+ *pid* +null+ +null+))


;; testing pass-by-reference
(defcfun ("passByReference" pbr) :void (x :pointer))

;; works:
;; (with-foreign-object (ptr :int 1)
;;   (pbr ptr)
;;   (print (mem-ref ptr :int)))

(defun am-i-root? ()
  (= (sb-posix:getuid) 0))


;; (defcstruct user-regs-struct :struct )

;; struct user_regs_struct
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
;; (defparameter *regs* (foreign-alloc  '(:struct user-regs-struct)))
;; (setf
;;  (foreign-slot-value *regs* '(:struct user-regs-struct) 'rax))
;; 42)

;; TODO: print regs, capture regs struct
(defun getregs (pid)
  (ptrace +ptrace-getregs+ pid +null+ +null+)
  ;;(ptrace +ptrace-getregs+ pid +null+ regs) ;; <- should be regs at the end!
  )

