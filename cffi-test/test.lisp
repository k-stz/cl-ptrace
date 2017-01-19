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
    (t (:default "~/sol_sanctum/cl-ptrace/bin/libtest")))

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

;; PTRACE_ATTACH = 16
;; PTRACE_DETACH = 17

(defconstant +PTRACE_ATTACH+ 16)
(defconstant +PTRACE_DETACH+ 17)
(defvar null-value (null-pointer))
(defconstant +NULL+ null-value)

;; (ptrace +PTRACE_ATTACH+ <pid-t> +NULL+ +NULL+)
;; (waitpid <pid-t> status 0)
;; (ptrace +PTRACE_DETACH+ <pid-t> +NULL+ +NULL+)

;; WORKS when Lisp is run as root!!!
;; NEXT-TODO: read from status pointer;
(defun attach-to (pid)
  (let ((status (null-pointer)))
    (print "ptrace ptrace_attach..")
    (ptrace +PTRACE_ATTACH+ pid +NULL+ +NULL+)
    (print "waitpid..")
    (waitpid pid status 0)
    ;; TODO how to retrieve statuscode from `status'
    (format t "attached to process PID: ~a " pid))
  pid)

;; add some datastructure to capture the state of a process, such as if it is already
;; traced, or ptrace returns a signal accordingly somehow if we try to detach from an
;; non-traced process?
;; (defun detach-from (pid)
;;   )
