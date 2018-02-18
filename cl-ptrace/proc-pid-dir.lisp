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

(defun has-pathname? (proc-pid-maps-line)
  "Returns the pathname of the parsed pid-maps-line, or if there is none, NIL."
  (getf proc-pid-maps-line :pathname))


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

(defun get-readable-non-pathname-list (proc-pid-maps-string-list)
  (loop for line in proc-pid-maps-string-list
     when (and (permission-readable? line)
	       (not (has-pathname? line)))
     collect line))

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
	     ;; 1+ is starting the sub-string after the hyphen
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


;; _Don't use this in a loop_, it is by a factor of 50 slower than a PEEKDATA call the only
;; advantage left to use this one is that we don't need to attach to a process thread
;; prior to using it.
(defun read-proc-mem-byte (address &key (pid *pid*) (hex-print? t))
  "Reads `address' from pid memory directly from /proc/pid/mem. 

This opens and closes the stream on each invocation, making it useful to inspect actual
current value under `address'"
  ;; Hack: Even though we just read from memory, if we don't make it an IO-Stream READ-BYTE
  ;; will raise an error I/O-Error when reading memory address 512 byte before the end of
  ;; a readable memory region...
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :io
		       :if-exists :append)
    (file-position str address)
    (let ((byte (read-byte str t)))
      (when hex-print?
      	(hex-print byte))
      byte)))

;; TODO: make it return the value as decimal, like READ-PROC-MEM-BYTE
(defun read-proc-mem-word (address &optional (pid *pid*))
  (loop for i from (+ address 7) downto address :do
       (format t "~(~2,'0x~)"
	       (read-proc-mem-byte i :pid pid :hex-print? nil)))
  (terpri))


(defun write-proc-mem-byte (address new-byte &key (pid *pid*))
  "Careful, this writes a `new-byte' to the process memory address of the process
designated by `pid'.
This can be used to write to any process memory, without having to trace or even
SIGSTOP it."
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
		       :if-exists :append)
    (file-position str address)
    (write-byte new-byte str)))

(defun write-proc-mem-word (address new-word &key (pid *pid*) (write-full-word? nil))
  "Writes the `new-word' to address, use `write-full-word?' to always write 8 bytes
regardless of leading zeros. Such that an new-word=#xabcd will write #x0000000000abcd,
instead of just #abcd and leaving the leading bytes as they where."
  (let ((bytes-to-write (integer-byte-length new-word)))
    (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
			 :if-exists :append)
      (file-position str address)
      (let ((0b (ldb (byte 8 0) new-word))
	    (1b (ldb (byte 8 8) new-word))
	    (2b (ldb (byte 8 16) new-word))
	    (3b (ldb (byte 8 24) new-word))
	    (4b (ldb (byte 8 32) new-word))
	    (5b (ldb (byte 8 40) new-word))
	    (6b (ldb (byte 8 48) new-word))
	    (7b (ldb (byte 8 56) new-word)))
	(write-sequence (list 0b 1b 2b 3b 4b 5b 6b 7b) str
			:end (if write-full-word?
				 8
				 bytes-to-write)))))
    (read-proc-mem-word address pid))


(defun print-proc-mem-table (&key address-list address-range (number-of-rows 30) (spacing 1) (pid *pid*))
  "Print Process memory addresses in a table. If `address-range' is provided it is used instead of
the address-list."
  (format  t "***PID: ~6a ~3a ~3a***~%"
	   pid number-of-rows spacing)
  (let ((row 1)
	(spaces (make-string spacing :initial-element #\Space)))
    (flet ((flet-print-memory (address)
	     (format t "~(~2x~)" (read-proc-mem-byte address :pid pid :hex-print? nil))
	     (format t "~a" spaces)
	     (when (= row number-of-rows)
	       (terpri) ;; new-line
	       (setf row 0))
	     (incf row)))
      (if (not address-range)
	  ;; address-list
	  (loop for address in address-list
	     :do
	       (flet-print-memory address))
	  ;; address-range
	  (loop for address from (first address-range) to (second address-range)
	     :do
	       (flet-print-memory address)))))
  (terpri))



(defun find-address-region-maps-entry (address &optional (pid *pid*))
  (flet ((address-in-address-region? (address-region)
	   (<= (first address-region) address (second address-region))))
    (loop for line in (parse-proc-pid-maps pid)
       :when
	 (address-in-address-region?
	  (address-range-list line))
       :do (return line))))

(defun find-address-region (address address-region-list)
  "Return the address-region where `address' is contained."
  (loop for address-region in address-region-list
     for from-address = (first address-region)
     for to-address = (second address-region)
     :when (<= from-address address to-address)
     :do (return address-region)))

