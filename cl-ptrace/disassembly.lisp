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


(defun collect-singlesteps-context (n-instructions &optional (pid *pid*))
  "Singlesteps for a given number of instructions and collects the instruction and its
  register state, at execution time, into a list of the form:
  (<instruction> <instruction-length> <register-context>)
  NOTE: Instruction length, and instruction isn't always right, see code comment!"
  (loop for n from 0 below n-instructions
     for rip-address = (rip-address)
     ;; TODO rewrite this
     ;; instruction is wrong, among possibly other situation, when: (1) instruction
     ;; is bigger than 8 bytes (because peekdata only fetched 8 bytes) and when (2) when a
     ;; jmp occured, as length calculation is a hack: current instruction address minus RIP
     ;; address.
     for instruction-length = (abs (- rip-address
				      ;; returns new rip-address!
				      (singlestep pid nil)))
     for instruction = (ldb (byte (* 8 instruction-length) 0)
			    (peekdata rip-address *pid* t nil))
     :collect
     ;; format: (<instruction> <length> <register-context>)
       (list 
	instruction instruction-length (get-registers)))))
