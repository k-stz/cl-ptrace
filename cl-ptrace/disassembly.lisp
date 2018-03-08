(in-package :cl-ptrace)

(defun get-bits (number from to &optional (binary-print? t))
  (assert (>= to from))
  (let ((bits (ldb
	       (byte (- (1+ to) from) from)
	       number)))
    (when binary-print?
      (binary-print bits))
    bits))



;; ModR/M Byte

(defun modr/m-fields (modr/m-byte)
  "Returns the three fields of a ModR/M Byte, as values."
  (values
   ;; Mod
   (get-bits modr/m-byte 6 7 t)
   ;; Reg/Opcode 
   (get-bits modr/m-byte 3 5 t)
   ;; R/M
   (get-bits modr/m-byte 0 2 t)))


;; Will return a list whose elements represent consequtive instructions
;; with address and register-state
;; TODO continue
(defun collect-singlesteps (n-instructions &optional (pid *pid*))
  (let ((first-instruction-address (rip-address pid)))
    ;; do one step because we don't know the address
    ;; of the instruction we start at. After this `singlestep' we know
    ;; the next and current instruction and can infer the instruction size
    ;; for non jump instructions
    (singlestep pid nil) 
    (loop for n from 0 upto n-instructions
       :collect
	 (singlestep pid nil))))
