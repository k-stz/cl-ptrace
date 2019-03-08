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
  "Singlesteps for `n-instructions' and collects the instruction and its
  register state, at execution time, into a list of the form:
  (<instruction> <instruction-length> <register-context>)
  NOTE: Instruction length, and instruction isn't always right, see code comment!"
  (loop for n from 0 below n-instructions
     for rip-address = (rip-address)
     ;; TODO rewrite this
     ;; among possibly other situation, instruction data is wrong when instruction
     ;; is bigger than 8 bytes (because peekdata only fetched 8 bytes), and when a jump
     ;; occured, the calculated length is also wrong calculation is a hack: current instruction address minus RIP
     ;; address.
     for address-distance = (abs (- rip-address
				    ;; returns new rip-address!
				    (singlestep pid nil)))
     ;; clamp max length to 8, when address-distance is to large this occurs
     ;; when a jmp instruction is issued.  
     for instruction-length = (if (<= address-distance 15)
				  address-distance
				  8)
     for instruction = (ldb (byte (* 8 instruction-length) 0)
			    (peekdata rip-address *pid* t nil))
     :collect
     ;; format: (<instruction> <length> <register-context>)
       (list 
 	instruction
	instruction-length
	(get-registers))))



(defun ml/push (register)
  "Return the machine code for the PUSH `register' operation"
  ;; PUSH machine code is: '0x5x'
  (case register
    (rax #x50)
    (rbp #x55)
    (rsi #x56)
    (rdi #x57)
    (t (progn
	 (error "Register not implemented. Can't generate machine code!")
	 #x50))))

(defun ml/pop (register)
  (case register
    (rax #x58)
    (rdi #x5f)
    (rsi #x5e)
    (rdx #x5a)
    (t (error "Register not implemented. Can't generate machine code!"))))

;; 48 b9 ff ff ff ff 01 00 00 00	movabs rcx,0x1ffffffff

(defun ml/movabs-rcx-0x1ffffffff (value)
  ;; #x48 b9 00 e4 0b 54 02 00 00 00
  ;; 0x2540be400
  ;; UPDATE: the instruction is laid out in memory as follows
  ;; (when viewed with (read-proc-mem-word...)
  ;; 0x00 00 00 01 ff ff ff ff b9 48
  ;; TODO we need to ensure the leading '00' are returned!
  ;; so hexabyte return value is wrong, we might have to use string..?
  "00 00 00 01 ff ff ff ff b9 48"
  )

(defun ml/mov-eax-0 ()
  "mov eax,0x0"
  #x00000000b8)

(defun ml/cmp (register ))
