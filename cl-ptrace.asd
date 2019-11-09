;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf-user)

(defsystem #:cl-ptrace
  :description "Common Lisp bindings to the `ptrace' Linux syscall, geared towards runtime 
process hacking through value injection"
  :version "0.1"
  :author "k-stz"
  :licence "MIT"
  :depends-on (:cffi :osicat)
  :serial t	
  :components
  ((:file "cl-ptrace/package")
   (:file "cl-ptrace/cl-ptrace")
   (:file "cl-ptrace/disassembly")
   (:file "cl-ptrace/proc-pid-dir")
   (:file "cl-ptrace/snapshot")
   (:file "cl-ptrace/utils")
   (:file "cl-ptrace/async-functions")))
