(in-package :cl-ptrace)

#+sbcl
(defun am-i-root? ()
  (= (sb-posix:getuid) 0))

(defun hex-print (number &optional (destination t) (padding nil))
  (if padding
      (format destination "~(~16x~)~%" number)
      (format destination "~(~x~)~%" number))
  number)

(defun binary-print (number &optional (destination t))
  (format destination "~b~%" number)
  number)

(defun address-list->peekdata-list (address-list &optional (pid *pid*))
  (loop for address in address-list
       :collect (peekdata address pid nil nil)))

#+sbcl
(defun permission-string->posix-permission-logior (permission-string)
  "Expects a permission-string of the form \"rwxp\", and returns the
LOGIOR of the permissions.
The logior of permission is needed by the syscalls like sb-posix:MMAP."
  (assert (= (length permission-string) 4))
  (let ((permission-logior))
    (setf permission-logior
	  (apply #'logior
		 (loop for character across permission-string
		    :collect (case character
			       (#\r sb-posix:prot-read)
			       (#\w sb-posix:prot-write)
			       (#\x sb-posix:prot-exec)
			       (#\p 0)
			       (#\s 0)
			       (#\- 0)
			       (t (error "~a is not a valid permission character." character))))))
    permission-logior))

#+sbcl
(defun permission-string->flag-private/shared (permission-string)
  "Expects a permission-string of the form \"rwxp\", and returns
the shared or private flag accordingly."
  (assert (= (length permission-string) 4))
  (let ((permission-flag-character (aref permission-string 3) ))
    (case permission-flag-character
      (#\s sb-posix:map-shared)
      (#\p sb-posix:map-private)
      (#\- (error "Permission-string ~a: you must provide a shared/private option 's' or 'p'"
		permission-string))
      (t (error "Permission-string: ~a is not a valid flag character, use 's' or 'p'."
		permission-string)))))

;; (defun address-list->peekdata-array (address-list &optional (pid *pid*))
;;   "Takes a list of addresses and saves there PEEKDATA output to an
;; array."
;;   (let ((array (make-array (length address-list))))
;;     (loop for address in address-list
;;        :for index from 0 do
;; 	 (setf (aref array index)
;; 	       (peekdata address pid nil nil)))
;;     array))

(defun count-address-data-match (address-list peekdata-list
				 &optional (count-matching? t) (pid *pid*))
  "Counts how many times the addresses in `address-list' point to the same
data (peekdata address ..) as the elements `peekdata-list' of the same index. 

Counts mismatches instead if `count-matching?' = nil!

Used in conjunction with the snapshot method on the first list of mismatches from
`find-mismatches'. "
  (assert (= (length address-list)
	     (length peekdata-list)))
  (loop for address in address-list for data in peekdata-list
     :count
     ;; decides here if always count matches or mismatches, using equivalency
       (equalp (= (peekdata address pid nil nil)
		  data)
	       count-matching?)))

(defun collect-address-data-match (address-list peekdata-list
				   &optional (collect-matching-p t) (pid *pid*))
  "Collects the addresses in `address-list' if they point to the same
data (peekdata address ..) as elements `peekdata-list' of the same index.

Collect mismatches instead if `collect-matching' = nil!

Note: Used in conjunction with the snapshot method."
  
  (assert (= (length address-list)
	     (length peekdata-list)))
  (loop for address in address-list for data in peekdata-list
     :when
     ;; equivalency
       (equalp (= (peekdata address pid nil nil)
		  data)
	       collect-matching-p)
     :collect address))


(defun stop-time (&optional (pid *pid*))
  "Sends a SIGSTOP to all the threads of a process."
  (with-foreign-object (status :int)
    (kill pid +sigstop+)
    ;; WUNTRACED only continues when process is stopped
    ;; TODO: this also blocks when issued twice?
    #+sbcl
    (waitpid pid status sb-posix:wuntraced)))

(defun stop (&optional (pid *pid*))
  (stop-time)
  (sleep 1)
  (attach-to pid))

(defun cont-time (&optional (pid *pid*))
  "Sends a SIGCONT to all the threads of a process."
  (kill pid +sigcont+))

(defun cont (&optional (pid *pid*))
  (detach-from pid)
  (cont-time))

;; move to util.lisp ?
(defun integer-byte-length (number)
  "How many bytes are needed to represent the number `number'"
  (let ((bytes (ceiling (integer-length number)
			 8)))
    (if (= bytes 0)
	1
	;; For the case number=0, it should return 1
	bytes)))


(defun address-range (address &optional (around 500))
  (let ((from-address (- address around))
	(to-address (+ address around)))
    (list
     (if (< from-address 0)
	 0
	 from-address)
     (if (> to-address (expt 2 64))
	 (expt 2 64)
	 to-address))))


;; (defun print-byte-region (from-address to-address &optional pid)
;;   (mapcar #'hex
;; 	  (loop for address :from from-address :to to-address collect
;; 	       (read-proc-mem-byte address :hex-print? nil))))

#+sbcl
(defun endianess ()
  (if (find :little-endian *features*)
      :little-endian
      :big-endian))

#+sbcl
;; This might be useful to free heap if we're close to running out due
;; to huge address-range-snapshot generations etc
;; use `(room)' to check the heap size. On SBCL it even
;; gives explicit size information about particular objects. For example
;; after allocating a memory-range-snapshot object I got the entry:
;;   3,679,024 bytes for     1,240 simple-array-unsigned-byte-64 objects.
(defun initiate-full-gc ()
  (sb-ext:gc :full t))


;; on #+sbcl (machine-type) will return the architecture!
;; (machine-type) => "X86-64"

;; some test functions

(defconstant +200m+ 200000000) ;; the problem size we need to deal with efficiently

(defun test (test-fn inverse-fn &optional (problem-size 300000) (compare-fn #'=))
  "Test if test-fn and its inverse are correct over inputs from 0 to  `problem-size'.
Return mismatching inputs, or true if all's right"
  (let (fail-input)
    (setf fail-input
	  (loop for i upto problem-size
	     :unless (funcall compare-fn
			      i
			      (funcall inverse-fn
				       (funcall test-fn i)))
	     collect i))
    (if (null fail-input)
	t
	(progn (format t "Failed for inputs:~%")
	       fail-input))))

(defun test-fn-equal (fn-1 fn-2 &optional (problem-size 30000) (equal-test-fn #'=))
  "Test if two functions return the same value from 0 to `problem-size' using
  `equal-test-fn' to compare the results"
  (declare (function fn-1 fn-2 equal-test-fn)
	   (fixnum problem-size))
  (let ((fail-input
	 (loop for i upto problem-size
	    :unless (funcall equal-test-fn
			     (funcall fn-1 i)
			     (funcall fn-2 i))
	    collect i)))
    (if (null fail-input)
	t
	(progn (format t "Failed for inputs:~%")
	       fail-input))))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))


;; from rosetta code
(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))


(defun bit-set? (number &optional (nth-bit 7))
 "Test if the `nth-bit' in `number' is set. Can be used to test if the
sign-bit is set"
  (if (= 1
	 (ldb (byte 1 nth-bit) number))
      t
      nil))

;; for disassembly

(defun extended-sign-mask (number-bits &optional (output-bits 64))
  "Creates a bit mask of '1's, the size `output-bits', where the last `number-bits' bits are zeros
Can be used to make sign-extended values, see `sign-extended-value'."
  ;; -1 = #xffffff.....ff (twos complement), so we use as bit-mask with shl, shift
  ;; -left. This leaves place to put the bits to be sign extended
  (shl (ldb (byte output-bits 0) -1) 
       output-bits number-bits))


(defun sign-extended-value (number &optional (output-bytes 8))
  "Bit extends numbers binary representation and returns value as decimal.

Used in disassembly when a byte like #xe4 gets binary extended to a quadword,
due to the instruction, to be used as a jump offset of -28. (twos complement
of #xe4 as a 1 Byte size value.) 
 (See JE, with opcode #x74)"
  (let ((bits (integer-length number)) ;; or most significant set bit+1
	 (bytes-of-number (integer-byte-length number))
	 (output-bits (* output-bytes 8)))
    (assert (>= output-bytes bytes-of-number))
    ;; test if last '1' bit is first in last byte -> negative number
    (if (bit-set? number (1- output-bits))
	;; negative number: sign extend with '1's
	(+ (extended-sign-mask bits output-bits)
	   number)
	;; positive number: sign-extend with '0's, i.e. do nothing,
	;; and return number
	number)))


(defun twos-complement (unsigned-value &optional (bytes 8))
  "Translate the `unsigned-value' given as an integer to its twos complement
representation.  Such that if the most significant bit is set, the negative value, of the
twos complement binary representation, is returned.
Example: (twos-complement #xff 1) ==> -1"
  (if (bit-set? unsigned-value (1- (* bytes 8)))
      (- (ldb (byte (* 8 bytes) 0)
	      (lognot (1- unsigned-value))))
      unsigned-value))

(defun mem-table (rows-delta &rest addresses)
  "Print Table with columns of `addresses' next to each other.
Used to find datastructures in memory by comparing different memory
regions from addresses that point to values that influence similar things.

`rows-delt' is the amount of bytes to print offset from the address.
If provided as a list: '(-32 64) then a variable offsets can be used."
  (assert (not (null addresses)))
  (let (start end)
    (if (listp rows-delta)
	(progn (setf start (first rows-delta))
	       (setf end (second rows-delta)))
	(progn (setf start (- rows-delta))
	       (setf end rows-delta)))
    (loop for i from start below end by 8 do
	 (loop for address in addresses do
	      (format t "~(~16x~)  "
		      (read-proc-mem-word (+ address i) 0 nil *pid*)))
	 (terpri))))

(defun print-around (address &optional (offset 64) (columns 1))
  (terpri)
  (format t "-----------------~%")
  (loop for loop-address from (- address offset) to (+ address offset) by 8
     with loop-col = columns do
     ;; special marker for address given
       (if (= loop-address address)
	   (format t ">")
	   (format t " "))
       (format t "~(~16x: ~16x~)  " loop-address (read-proc-mem-word loop-address 0 nil))
     ;; table column logic:
       (decf loop-col)
       (when (<= loop-col 0)
	 (setf loop-col columns)
	 (terpri))))

(defun print-live (address &optional (offset 64) (columns 1) (sleep-seconds 1))
  (loop
     (sleep sleep-seconds)
     (print-around address offset columns)))
