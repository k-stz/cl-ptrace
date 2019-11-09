(in-package :cl-ptrace)

;;; Functions that work without having to stop the process and attaching to it

(defvar *sieve* nil
  "Used by default by address value search function to collect the result.")

(defun %async-find-value-heap (value &optional (pid *pid*))
  (with-snapshot (heap-snapshot (get-heap-address-range pid))
    (let* ((start-address (slot-value heap-snapshot 'start-memory-address))
	   (end-address (slot-value heap-snapshot 'end-memory-address))
	   (value-byte-array (get-byte-array (make-mem-array value)))
	   (value-byte-size (length value-byte-array))
	   (first-value-byte (aref value-byte-array 0))
	   result-list)
      (setf result-list
	    ;; for efficency: in case the value is just one byte big don't do the full
	    ;; byte-array comparison
	    (if (= value-byte-size 1)
		(loop :for address :from start-address :below end-address
		   :when
		     (=
		      (snapshot-read-byte heap-snapshot address)
		      first-value-byte)
		   :collect address)
		;; "below" here is correct, i.e. end-memory-address is _exclusive_!
		(loop :for address :from start-address :below end-address
		   :when
		     (%snapshot-mem-equalp heap-snapshot address
					   value-byte-array
					   value-byte-size)
		   :collect address))))))

(defun gen-internal-var (name-string value)
  "Returns a uniquely named variable whose name contains `name-string' and its value set to `value'"
  (let ((new-var (make-symbol
		  (string-upcase
		   (format nil "*~a*"
			   (symbol-name
			    (gensym
			     (format nil "~a-" name-string))))))))
    (import new-var)
    ;;  `SET' is the right thing here, since it explicitly sets the value-cell of the new
    ;;  symbol!
    (set new-var value)
    new-var))


(defun async-find-value-heap (value &optional (result-string nil) (pid *pid*))
  "Search the heap for `value' (can be number, hex-string or byte sequence), and
save the result in uniquely named variable that whose name contains `result-string', if none is provided save result in `*sieve*'."
  (let ((result (%async-find-value-heap value pid)))
    (format t "found: ~a" (length result))
    (if (null result-string)
	(setf *sieve* result)
	(gen-internal-var result-string result))))


;; TODO: implement "padding" (see find-value-address) probably through masking the
;; memory-array; implement address-list search
(defun async-find-value-address (value &key (pid *pid*)
					 (address-list nil))
  (let* ((value-byte-array (get-byte-array (make-mem-array value)))
	 (length-value-byte-array (length value-byte-array)))
    (when address-list
      (loop for address in address-list
	 ;; TODO: efficientcy can be improved: open file once and search it, the read bytes
	 ;; might be read into a byte-buffer rather then a list? (see read-mem implementation)
	 :when (equalp ;; test byte-equalp again
		value-byte-array
		(get-byte-array
		 (read-mem address length-value-byte-array pid)))
	 collect address))))
