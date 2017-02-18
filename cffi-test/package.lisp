;; pcl advise ; obsolete once ASDF is used?
;; avoids risk of interning symbols in some other package, from whom this file is loaded
;; or compiled
(in-package "COMMON-LISP-USER")

(defpackage :cffi-user
  (:use :common-lisp :cffi))

