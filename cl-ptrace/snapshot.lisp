(defpackage :cl-ptrace
  (:use :common-lisp :cffi))

(in-package :cl-ptrace)

;; #include <sys/uio.h>
;; ssize_t process_vm_readv(pid_t pid,
;;                          const struct iovec *local_iov,
;;                          unsigned long liovcnt,
;;                          const struct iovec *remote_iov,
;;                          unsigned long riovcnt,
;;                          unsigned long flags);
;; Where local_iov is a struct from <sys/uio.h>:
;; struct iovec {
;;                void  *iov_base;    /* Starting address */
;;                size_t iov_len;     /* Number of bytes to transfer */
;;            };
(defctype size-t :unsigned-long)

(defcstruct iovec
  (iov-base :pointer) ; starting address, type void!
  (iov-len size-t)) ; number of bytes to transfer

(defcfun "process_vm_readv" :long
  (pid pid-t)
  (local-iovec :pointer)     ; local datastructure to capture remote process memory
  (local-iovec-count :unsigned-long) ; const struct iovec *local_iov,
  (remote-iovec :pointer)    ; REMOTE: process specified by PID, data to be transferred
  (remote-iovec-count :unsigned-long) ; const struct iovec *remote_iov,
  (flags :unsigned-long))  ; currently unused, must be set to 0

(defun %alloc-iovec-struct (iov-base iov-len)
  "Allocate and return a the foreign C-struct: `IOVEC' which is needed by the syscall
process_vm_readv. 
It's fields show from which address (base-iov) to read how many elements
(len-iov). But it is also used to save the data from from another process
in an array (base-iov), namely len-iov many bytes.
For the two uses see the signature of the syscall."
  ;; all the data shall be pulled from and written to a single large buffer, so this
  ;; is hardcoded with: count = 1 and
  (let ((iovec-struct (foreign-alloc '(:struct iovec) :count 1)))
    ;; iovec.iov-base
    (setf (foreign-slot-value iovec-struct '(:struct iovec) 'iov-base)
	  (foreign-alloc :unsigned-char :count iov-base))
    ;; iovec.iov-len
    (setf (foreign-slot-value iovec-struct '(:struct iovec) 'iov-len)
	  iov-len)
    iovec-struct))

(defun iovec-get-iov-base (iovec-struct)
  (foreign-slot-value iovec-struct '(:struct iovec) 'iov-base))

(defun iovec-get-iov-len (iovec-struct)
  (foreign-slot-value iovec-struct '(:struct iovec) 'iov-len))

(defmacro iov-base-mem-ref (iovec-struct index)
  (mem-ref (iovec-get-iov-base iovec-struct) :unsigned-char index))


;; surprisingly scanning over a c-array isn't anywhere slower than a lisp-array
;; this should be kept in mind before using this function for efficiency reasons
(defun iovec->lisp-array (iovec)
  ;; cffi:foreign-array-to-lisp creates a simple-vector array, without
  ;; a specific element type, so we make an own function
  ;; (foreign-array-to-lisp iov-base
  ;; 			 (list :array :unsigned-char array-length))
  (let* ((array-length (1+ (iovec-get-iov-len iovec)))
	 (array (make-array array-length ))
	 (iov-base (iovec-get-iov-base iovec)))
    (loop for index from 0 below array-length do
	 (setf (aref array index)
	       (mem-ref iov-base :uint8 index)))
    array))

;; because we need to foreign-free all that is foreign-alloc'ated, and the struct has the
;; field `base-len', which was foreign-alloc'ated, we use this function to conventietly
;; free it
(defun %free-iovec-struct (iovec-struct)
  (foreign-free
   (foreign-slot-value iovec-struct '(:struct iovec) 'iov-base))
  (foreign-free iovec-struct))

;; TODO: build snapshot start-address = 0 index abstraction on top of it
;;            see that you provide means to free the pointer, also
;;            since currently the base-len of the iovec is returned, which is
;;            just a field in the iovec struct next to the iov-length!
(defun process-vm-readv-into-iovec (tracee-address-range &key (pid *pid*))
  "Copy the bytes in tracee-address-range into the tracer process, and return a SAP,
system area pointer, to it.
Uses the syscall process_vm_readv.
Doesn't require ptrace attachment, or stopping the tracee process."
  (let* ((size-tracee-address-range
	  ;; 1- because the end-address in the address-range representation is
	  ;; exclusive
	  (1- (address-range-length tracee-address-range)))
	 (local-iovec
	  (%alloc-iovec-struct size-tracee-address-range
			       size-tracee-address-range))
	 ;; `remote-iov-count' is an array of IOVEC structs, so we can request multiple memory
	 ;; regions, but we'll work with copying one memory address range for now, so:
	 (remote-iov-count 1)
	 ;; `local-iov' describes the local, tracer memory, address where to write it to.
	 ;; where the field `iov-len' descibes the size of the buffer and `local-iov-count'
	 ;; the number of buffers. We will use one buffer to write it to, not splitting it,
	 ;; thus:
	 (local-iov-count 1)
	 ;; on success the syscall process-vm-readv returns the amount of read bytes, we
	 ;; store them in:
	 (number-of-read-bytes)) 
    (with-foreign-object (remote-iovec '(:struct iovec))
      ;; creating remote-iovec:
      ;; set iov-base
      (setf (foreign-slot-value remote-iovec '(:struct iovec) 'iov-base)
	    (make-pointer (first tracee-address-range)))
      ;; set iov-len
      (setf (foreign-slot-value remote-iovec '(:struct iovec) 'iov-len)
	    size-tracee-address-range)

      (setf number-of-read-bytes
	    (process-vm-readv pid
			      local-iovec local-iov-count
			      remote-iovec remote-iov-count
			      0))
      (if (= -1 number-of-read-bytes) ;; syscall returning -1, means error occured:
	  (format t "~a~%"(strerror))
	  (format t "process-vm-readv syscall, number of read bytes: ~a" number-of-read-bytes))
      ;; return pointer to freshly transferred memory:
      local-iovec)))


;;Snapshot----------------------------------------------------------------------

;; This will store the values of a memory range at a the time. That's what is implied
;; by "snapshot" this won't be used to retrieve up-to-date values or to even set
;; any value. This should be treated as a read-only object after the slots have been set
(defclass memory-range-snapshot ()
  ((start-memory-address :initarg :start-memory-address)
   (end-memory-address :initarg :end-memory-address)
   (pid :initarg :pid)
   ;; use to free memory, when memory-range-snapshot is not needed
   (snapshot-memory-iovec :initarg :snapshot-memory-iovec)
   (snapshot-memory-c-array :initarg :snapshot-memory-c-array :accessor snapshot-c-array)
   ;; from old implementation:
   (snapshot-memory-array :initarg :snapshot-memory-array :accessor snapshot-memory-array)))

(defgeneric get-memory-range (memory-range-snapshot))
(defmethod get-memory-range ((obj memory-range-snapshot))
  (with-slots (start-memory-address end-memory-address) obj
    (list start-memory-address end-memory-address)))

(defmethod print-object ((obj memory-range-snapshot) stream)
  (let ((memory-range (get-memory-range obj)))
    (format stream "#<MS:[~(~x~)-~(~x~)]>"
	    (first memory-range)
	    (second memory-range))))



;; NEXT TODO rewrite after neo-make-snapshot-memory-range works (read 8 bytes in a row)
(defgeneric snapshot-peekdata (memory-range-snapshot address))
(defmethod snapshot-peekdata ((obj memory-range-snapshot) address)
  ;; This just maps calls like (aref-mem-snapshot obj <start-address>+n) to internally
  ;; (aref obj.array 0+n)
  (with-slots (snapshot-memory-array start-memory-address) obj
    (declare (type (vector (unsigned-byte 64)) snapshot-memory-array)
	     ((unsigned-byte 64) start-memory-address address))
    (aref snapshot-memory-array
	  (- address start-memory-address))))

;; NEXT-TODO 
;; (defgeneric snapshot-read-byte (memory-range-snapshot address))
(defmethod neo-snapshot-read-byte ((obj memory-range-snapshot) address)
  "Return the byte in the memory-snapshot using the address that was referring to its original
in from the emory region the snapshot copied (from that remote process address space)"
  (with-slots (snapshot-memory-c-array start-memory-address) obj
    (mem-ref snapshot-memory-c-array :unsigned-char (- address start-memory-address))))


;; TODO: currently the snapshot stores under each address the entire WORD `PEEKDATA'
;; returns. That means for each single address 8 bytes are stored. Use
;; (read-proc-mem-byte) to create the snapshot, and then let (snapshot-peekdata ..) mimic
;; the behaviour of reading 8 bytes in a row!
(defun make-snapshot-memory-range (&key from-address to-address address-range (pid *pid*))
  (when address-range
    (setf from-address (first address-range)
	  to-address (second address-range)))
  (labels ;; don't call directly, use `snapshot-memory-range' instead!
      ((make-snapshot-instance
	   (snapshot-memory-array start-address end-address &optional (pid *pid*))
	 (make-instance 'memory-range-snapshot
			:start-memory-address start-address
			:end-memory-address end-address
			:pid pid
			:snapshot-memory-array snapshot-memory-array)))

    (with-open-file (mem-stream (get-mem-path pid) :direction :input :element-type '(unsigned-byte 8))
      (file-position mem-stream from-address)
      
      (let ((snapshot-memory-array (make-array (1+ (- to-address from-address))
					       :element-type '(unsigned-byte 64))))
	(loop for mem-byte :from from-address :to to-address
	   for array-index from 0
	   :do
	     (setf (aref snapshot-memory-array array-index)
		   (peekdata mem-byte pid nil nil)))
	(make-snapshot-instance snapshot-memory-array from-address to-address pid)))))

;; NEXT-TODO:
;; implement in terms of or use (process-vm-readv-address-range ..)
;; since process-vm-readv is very fast, it makes sense to always take a snapshot
;; instead of scanning the memory with peekdata!!
(defun neo-make-snapshot-memory-range (&key from-address to-address address-range (pid *pid*))
  (when address-range
    (setf from-address (first address-range)
	  to-address (second address-range)))
  (labels ;; don't call directly, use `snapshot-memory-range' instead!
      ((make-snapshot-instance
	   (snapshot-memory-iovec start-address end-address &optional (pid *pid*))
	 (make-instance 'memory-range-snapshot
			:start-memory-address start-address
			:end-memory-address end-address
			:pid pid
			:snapshot-memory-iovec snapshot-memory-iovec
			:snapshot-memory-c-array (iovec-get-iov-base snapshot-memory-iovec))))
    (let ((local-iovec
	   (process-vm-readv-into-iovec (list from-address to-address))))
      (make-snapshot-instance local-iovec from-address to-address pid))))


(defun test-neo-snapshot (memory-range-snapshot)
  (with-slots (start-memory-address end-memory-address pid) memory-range-snapshot
    (loop for address from start-memory-address to end-memory-address
       :always
	 (= (read-proc-mem-byte address :pid pid :hex-print? nil)
	    (snapshot-read-byte memory-range-snapshot address)))))


;; TODO make macro hygienic
(defmacro loop-snapshot ((address-var memory-range-snapshot) &body body)
  `(with-slots (start-memory-address end-memory-address) ,memory-range-snapshot
     (loop for ,address-var from start-memory-address to end-memory-address
	  ,@body)))

(defun find-mismatches (memory-range-snapshot &optional (pid *pid*))
  "Returns list of all addresses of `snapshot' whose entries mismatch with 
the (peekdata addr pid ..) entires, at the same address."
  (loop-snapshot (address memory-range-snapshot)
     when (not (= (peekdata address pid nil nil)
		  (snapshot-peekdata memory-range-snapshot address)))
     collect address))
