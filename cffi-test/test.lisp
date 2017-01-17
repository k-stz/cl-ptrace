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
  ;; (:unix (:or (;"libc-2.19"
  ;; 	       "libc-2.19.so"
  ;; 	       "/lib/x86_64-linux-gnu/libc-2.19"
  ;; 	       "/lib/x86_64-linux-gnu/libc.so.6"
  ;; 	       "/usr/lib/x86_64-linux-gnu/libc.so"
  ;; 	       "libc.so.6" "libc.so" "libc-2.24.so" "libc.a"
  ;; 	       "/usr/lib/libc-2.24.so" "/usr/lib/libc-2.24")))
  ;; TODO: how to actually load differnt versions with (:user (:OR ..)) ??
  ;; ubuntu:
  (t (:default  "/lib/x86_64-linux-gnu/libc-2.19")))


;; NEXT-TODO:
;; from here on we might just need to work with the header files
;; sys/ptrace.h

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
