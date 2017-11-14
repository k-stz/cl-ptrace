(in-package :cl-ptrace)

#+sbcl
(defun am-i-root? ()
  (= (sb-posix:getuid) 0))

(defun hex-print (number &optional (destination t))
  (format destination "~(~x~)~%" number))

(defun address-list->peekdata-list (address-list &optional (pid *pid*))
  (loop for address in address-list
       :collect (peekdata address pid nil nil)))

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

Used in conjunction with the snapshot method on the first list ofmismatches from
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
  (kill pid +sigstop+))

(defun cont-time (&optional (pid *pid*))
  "Sends a SIGCONT to all the threads of a process."
  (kill pid +sigcont+))

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

