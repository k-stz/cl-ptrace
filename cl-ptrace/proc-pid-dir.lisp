(in-package :cl-ptrace)

;;; /proc/<pid>/maps operations:

;; well this is useless, it corresponds with /proc/<pid>/maps but just
(defun find-readable-memory (from-num to-num &optional (pid *pid*))
  (let ((first-readable nil)
	(last-readable from-num)
	(start-set? nil))
    (loop for address from from-num to to-num  
       for peeked-data = (peekdata address pid nil nil) do
	 (if (ptrace-successful? peeked-data nil)
	     ;; success
	     (when (not start-set?)
	       (setf first-readable address) 
	       (setf start-set? t))
	     ;; not readable
	     (when start-set?
	       (setf start-set? nil)
	       (setf last-readable address)
	       (format t "Hex:     readable: ~(~10,x~) - ~(~10,x~)~%" first-readable last-readable))))
    ;; left the loop, now either no readable found, or no unreadable found yet
    (when start-set?
      (format t "Hex:     readable: ~(~10,x~) - ~(~10,x~)~%" first-readable to-num))))

(defun get-maps-path (&optional (pid *pid*))
  (concatenate 'string
	       "/proc/"
	       (format nil "~a" pid)
	       "/maps"))

(defun parse-proc-pid-maps (&optional (pid *pid*) (parse-this-file-instead nil))
  "Return a list of plists with GETFable columns of /proc/pid/maps"
  (let (maps-line-strings
	(file-to-parse
	 (if parse-this-file-instead
	     parse-this-file-instead
	     (get-maps-path pid))))
    (setf maps-line-strings
	  (with-open-file (maps-stream file-to-parse :direction :input)
	    ;; condition of type END-OF-FILE
	    (loop for text = (read-line maps-stream nil nil) 
	       while text ;; nil
	       collect text)))
    (loop for line in maps-line-strings collect 
	 (with-input-from-string (string-stream line)
	   (destructuring-bind (address-range permissions offset dev inode &optional pathname)
	       (loop for i from 1 to 6
		  :if (< i 6) 
		  :collect (read-word-to-string string-stream)
		  :else
		          ;; quick hack "         /some/path/etc" -> "/some/path/etc
		  :collect (remove #\Space (read-line string-stream nil nil)))
	     (list :address-range address-range
		   :permission permissions
		   :offset offset
		   :dev dev
		   :inode inode
		   :pathname pathname))))))


(defun permission-readable? (proc-pid-maps-line)
  "Takes a string like 'rw-p' and returns true if 'r' is set "
  (let ((permission-string (getf proc-pid-maps-line :permission)))
    (char= #\r (aref permission-string 0))))

(defun get-heap-address-range (&optional (pid *pid*))
  (loop for line in (parse-proc-pid-maps pid) :do
       (when
	   (string= "[heap]"
		    (getf line :pathname))
	 (return (progn
		   (hex-print
		    (address-range-list line))
		   (address-range-list line))))
     :finally (error "Process maps file has no [heap] entry. PID: ~a" pid)))

;; Takes the output from `parse-proc-pid-maps' and creates a
;; list of memory regions that are all readable
(defun get-readable-memory-regions (proc-pid-maps-string-list)
  "Return a list of all readable address ranges from a parsed /proc/pid/maps
file. `proc-pid-maps-string-list' should be the output of `parse-proc-pid-maps'"
  (loop for line in proc-pid-maps-string-list
     when (permission-readable? line)
     collect (address-range-list line)))

(defun address-range-list (proc-pid-maps-line)
  (let ((address-range (getf proc-pid-maps-line :address-range))
	start-address
	end-address)
    (multiple-value-bind (left-address index-end) (parse-integer address-range
								 :radix 16
								 :junk-allowed t)
      (setf start-address left-address)
      (setf end-address
	    (parse-integer
	     ;; 1+ is starting the substring after the hyphen
	     ;; 0400000-50000
	     ;;        ^ this hyphen, after the '040000' part has been parsed
	     (subseq address-range (1+ index-end))
	     :radix 16)))
    (list start-address end-address)))

(defun address-range-length (address-range)
  "Return the number of addresses in given `address-range'"
  (abs (- (first address-range)
	  (second address-range))))

(defun read-word-to-string (stream)
  (let ((char-list '()))
    (loop
       for char = (read-char stream nil nil) do
	 (cond ((null char)
		(return))
	       ((or (char= char #\Tab)
		    (char= char #\Space))
		(return))
	       (t
		(push char char-list))))
    (coerce (reverse char-list) 'string)))


;;; /end /proc/<pid>/maps operations


(defun get-mem-path (&optional (pid *pid*))
  "Return the path to /proc/<pid>/mem"
  (concatenate 'string
	       "/proc/"
	       (format nil "~a" pid)
	       "/mem"))

(defun read-proc-mem-byte (address &key (pid *pid*) (hex-print? t))
  "Reads `address' from pid memory directly from /proc/pid/mem. 

This opens and closes the stream on each invokation, making it useful to inspect actual
current value under `address'"
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8))
    (file-position str address)
    (let ((byte (read-byte str)))
      (when hex-print?
	(hex-print byte))
      byte)))


(defun write-proc-mem-byte (address new-byte &key (pid *pid*))
  "Careful, this writes a `new-byte' to the process memory address of the process
designated by `pid'.
This can be used to write to any process memory, without having to trace or even
SIGSTOP it."
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
		       :if-exists :append)
    (file-position str address)
    (write-byte new-byte str)))


(defun print-proc-mem-table (address-list &optional (number-of-rows 10) (pid *pid*))
  (let ((row 1))
    (loop for address in address-list
       :do
	 (format t "~(~x~)" (read-proc-mem-byte address :pid pid :hex-print? nil))
	 (when (= row number-of-rows)
	   (terpri) ;; new-line
	   (setf row 1))
	 (incf row))))


;; This will store the values of a memory range at a the time. That's what is implied
;; by "snapshot" this won't be used to retrieve up-to-date values or to even set
;; any value. This should be treated as readonly object after the sltos have been set
(defclass memory-range-snapshot ()
  ((start-memory-address :initarg :start-memory-address)
   (end-memory-address :initarg :end-memory-address)
   (pid :initarg :pid)
   (snapshot-memory-array :initarg :snapshot-memory-array :accessor snapshot-memory-array
			  :type (array (unsigned-byte 64) 1))))

(defgeneric get-memory-range (memory-range-snapshot))
(defmethod get-memory-range ((obj memory-range-snapshot))
  (with-slots (start-memory-address end-memory-address) obj
    (list start-memory-address end-memory-address)))

(defmethod print-object ((obj memory-range-snapshot) stream)
  (let ((memory-range (get-memory-range obj)))
    (format stream "#<MS:[~(~x~)-~(~x~)]>"
	    (first memory-range)
	    (second memory-range))))



;; TODO: add declaration or slot type size of snapshot array length being of type '(unsigned-byte 64)
;;       do some renaming and perhaps hide some functions that should never be used
;;       on their own but to created snapshots of memory-ranges
(defgeneric snapshot-peekdata (memory-range-snapshot address))
(defmethod snapshot-peekdata ((obj memory-range-snapshot) address)
  ;; This just maps calls like (aref-mem-snapshot obj <start-address>+n) to internally
  ;; (aref obj.array 0+n)
  (with-slots (snapshot-memory-array start-memory-address) obj
    (declare (type (vector (unsigned-byte 64)) snapshot-memory-array)
	     ((unsigned-byte 64) start-memory-address address))
    (aref snapshot-memory-array
	  (- address start-memory-address))))


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

(defun find-address-region (address address-region-list)
  "Return the address-region where `address' is contained."
  (loop for address-region in address-region-list
     for from-address = (first address-region)
     for to-address = (second address-region)
     :when (<= from-address address to-address)
     :do (return address-region)))
