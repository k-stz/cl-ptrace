(in-package :cl-ptrace)


(defun am-i-root? ()
  (= (sb-posix:getuid) 0))

(defun hex-print (number &optional (destination t))
  (format destination "~(~x~)~%" number))


#+sbcl
(defun endianess ()
  (if (find :little-endian *features*)
      :little-endian
      :big-endian))

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
