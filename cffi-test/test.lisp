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
  ;; (:unix (:or ("libc.so.6" "libc.so" "libc-2.24.so" "libc.a" "/usr/lib/libc-2.24.so" "/usr/lib/libc-2.24")))
  (t (:default "/usr/lib/libc-2.24")))


;; NEXT-TODO:
;; from here on we might just need to work with the header files
;; sys/ptrace.h

;; this effectively loads libc into the Lisp image much, just like we load .lisp files
;; into the lisp image
(use-foreign-library libc)


