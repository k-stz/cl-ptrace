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

;; TODO more tests, fairly slow might have to filter already
;; while collecting
(defun collect-singlesteps (n-instructions &optional (pid *pid*))
  (loop for n from 0 below n-instructions
     for rip-address = (rip-address)
     for address-length = (abs (- rip-address
				   ;; returns new rip-address!
				   (singlestep pid nil)))
     :collect
     ;; format: (<instruction-addr> <register-state-at-time-of-instruction>)
       (list rip-address address-length (get-registers)))))

;; ;; TODO continue
;; 
;; (loop for x in (collect-singlesteps 10) collect
;; 		(ldb (byte (* 8 (second x)) 0)
;; 		     (peekdata (first x) *pid* nil t)))
