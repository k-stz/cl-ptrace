(in-package :cl-ptrace)

;;; /proc/<pid>/maps and
;;; /proc/<pid>/mem operations

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

(defun permission-private? (proc-pid-maps-line)
  "Takes a string like 'rw-p' and returns true if 'r' is set "
  (let ((permission-string (getf proc-pid-maps-line :permission)))
    (char= #\p (aref permission-string 3))))


(defun has-pathname? (proc-pid-maps-line)
  "Returns the pathname of the parsed pid-maps-line, or if there is none, NIL."
  (getf proc-pid-maps-line :pathname))


(defun get-heap-address-range (&optional (pid *pid*))
  "Get the limits of the heap for the process referred to by `*pid*'. The end address is NOT inclusive,
and should not be read from."
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
;; TODO find more criteria for 'useless' memory regions, it seems that when
;; they have a :dev entry or :pathname they're probably loaded from somewhere other than
;; the binary for example (just a some driver or static library)
(defun get-readable-memory-regions (proc-pid-maps-string-list &optional (without-pathname? nil) (without-heap? nil))
  "Return a list of all readable address ranges from a parsed /proc/pid/maps
file. `proc-pid-maps-string-list' should be the output of `parse-proc-pid-maps'"
  (loop for line in proc-pid-maps-string-list
     when (and (permission-readable? line)
	       (permission-private? line)
	       (if without-pathname?
		   (if (stringp (getf line :pathname))
		       nil
		       t)
		   t)
	       (if without-heap?
		   (if (string= "[heap]" (getf line :pathname))
		       nil
		       t)
		   t))
     collect (address-range-list line)))

(defun get-readable-non-pathname-list (proc-pid-maps-string-list)
  (loop for line in proc-pid-maps-string-list
     when (and (permission-readable? line)
	       (not (has-pathname? line)))
     collect line))

(defun address-range-list (proc-pid-maps-line)
  "Returns an address-range object representing the /proc/<pid>/maps/ entry, the seconds
address is NOT inclusive and should not be read from."
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
(defun read-proc-mem-byte (address &key (bytes 1) (pid *pid*) (hex-print? t))
  "Reads `address' from pid memory directly from /proc/pid/mem. 

This opens and closes the stream on each invocation, making it useful to inspect actual
current value under `address'"
  ;; Hack: Even though we just read from memory, if we don't make it an IO-Stream READ-BYTE
  ;; will raise an error I/O-Error when reading memory address 512 byte before the end of
  ;; a readable memory region...
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :io
		       :if-exists :append)
    (file-position str address)
    (let ((byte
	   (read-byte str t)))
      (when hex-print?
      	(hex-print byte))
      byte)))

(defun n-read-proc-mem-bytes-list (address &key (bytes 1) (pid *pid*))
  "Read `bytes' amount under `address' from process memory and return a list in of the
bytes in a address-ascending order."
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :io
		       :if-exists :append)
    (file-position str address)
    (loop for address from address below (+ address bytes)
       ;; TODO: what happens if we read outside of memory segment, do we want to
       ;; check for that here?	
       :collect (read-byte str t))))

;; once this works, replace other read-proc-* functions
(defun read-mem (address &optional (bytes 8) (pid *pid*))
  "Read `bytes' amount under `address' from process memory and return a `memory-array'
representation.  Can be used without attaching or stopping the target process referred to
by `pid'."
  ;; TODO: if :direction is not only :input will this hinder parallelization?
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :io
		       :if-exists :append)
    (file-position str address)
    (make-mem-array
     (loop for address from address below (+ address bytes)
	:collect (read-byte str t))
     address)))

;; TODO: allow writing half-bytes as well?
;; currently writing "f" will write => "0f" to memory!
;; could be done by using already a mask?
(defun write-mem (address value &key (pid *pid*))
  "Write the `value' given to the process memory starting from `address' in
sequential order.

`value' will be internally treated as input to a `memory-array' (make-mem-array ..) such that:

`value' can be represented as an integer (write only the bytes needed to represent it),
hex-string (allows for leading zeros) or a byte-sequence like #(32 172) and '(255 312).

Returns a `memory-array' representing the newly changed memory."
  (let ((value-byte-array
	 (get-byte-array
	  (make-mem-array value nil))))
    (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
			 :if-exists :append)
      (file-position str address)
      (write-sequence value-byte-array str
		      :end (length value-byte-array)))
    (read-mem address (max (length value-byte-array) 8))))

(defun n-write-proc-mem-bytes-list (address byte-list &key (pid *pid*))
  "Write bytes in the byte-list given to the process memory starting from `address' in
sequential order."
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
		       :if-exists :append)
    (file-position str address)
    (write-sequence byte-list str
		    :end (length byte-list))))

(defun n-read-proc-mem (address &optional (bytes 8) (pid *pid*))
  (byte-list->number
   (n-read-proc-mem-bytes-list address :bytes bytes :pid pid)))

(defun read-proc-mem-word (address &optional (offset 0) (hex-print? t) (pid *pid*))
  (let* ((integer-word
	  (n-read-proc-mem (+ offset address) 8 pid)))
    (when hex-print?
      (hex-print integer-word t t))
    integer-word))


;; use this with Disassembly!
;; behaves same as above... TODO
(defun byte-list->number (byte-list)
  "Converts a list of bytes like (255 255 40 77 46 41 0 96), to
an integer of those 8 bytes, in this example: #x6000292e4d28ffff"
  (apply #'+
	 (loop for index from 0 below (length byte-list)
	    for byte in byte-list
	    :collect
	      (ash byte (* 8 index)))))


(defun byte-list= (byte-list1 byte-list2)
  (unless (= (length byte-list1) (length byte-list2))
    (error "Byte lists don't have same length: ~a ~a" byte-list1 byte-list2))
  (loop
     :for byte1 in byte-list1
     :for byte2 in byte-list2
     :always (= byte1 byte2)))

(defun integer->byte-list (integer)
  "Converts a integer into a list of bytes, in this case: Example #x6000292e4d28ffff
  => (255 255 40 77 46 41 0 96)\""
  (let ((number-byte-length (integer-byte-length integer)))
    (loop for byte from 0 below number-byte-length
       :collect
	 (ldb (byte 8 (* byte 8)) integer))))

(defun hex-string->byte-list (hex-string)
  "Translates a string like \"0001ff02\" or \"#x0001ff02\" to the byte-list
   (2 1 255 0)."
  (unless (stringp hex-string)
    (error "~a is not a string." hex-string))
  (flet ((sanitize-hex-string (hex-string)
	   (if (>= (length hex-string) 2)
	       (if (string= "#x" (string-downcase (subseq hex-string 0 2)))
		   ;; cut own the #x or #X from the beginning of string
		   (subseq hex-string 2)
		   hex-string)
	       hex-string)))
    (let*
	((sanitized-hex-input (sanitize-hex-string hex-string))
	 (hex-list (split-sequence-backwards-by-n sanitized-hex-input 2)))
      (mapcar (lambda (hex)
		(parse-integer hex :radix 16))
	      hex-list))))

(defun pad-byte-list (byte-list padding-length)
  (let ((pad-diff (- padding-length (length byte-list))))
    (if (plusp pad-diff)
	(append byte-list (make-list pad-diff :initial-element 0))
	byte-list)))

(defun get-byte (number byte)
  (ldb (byte 8 (* byte 8)) number))

#+sbcl
(defun ascii-string->integer (string)
  (let ((byte-list
	 (loop for char across string
	    :collect (char-code char))))
    (byte-list->number byte-list)))

;; TODO use flexi-streams
#+sbcl
(defun integer->ascii-string (integer)
  (let* ((byte-length (integer-byte-length integer))
	 (string (make-array byte-length :element-type 'character)))
    (loop for i from 0 below byte-length :do
	 (setf (aref string i)
	       (code-char (ldb (byte 8 (* i 8)) integer))))
    string))

(defun write-proc-mem-byte (address new-byte &key (pid *pid*))
  "Careful, this writes a `new-byte' to the process memory address of the process
designated by `pid'.
This can be used to write to any process memory, without having to trace or even
SIGSTOP it."
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
		       :if-exists :append)
    (file-position str address)
    (write-byte new-byte str)))

;; TODO still doesn't write #x00ab <- leading zeros, because of (integer-byte-length ..)
;; use. Given that we pass it a hex representation like #x00ab, either this is solved
;; via macro, another option word, another input type (string?) or get in the habit
;; of writing some non zero byte before it.
(defun write-proc-mem-word (address new-word &key (pid *pid*) (write-full-word? nil)
					       (hex-print? t))
  "Writes the `new-word' to address, use `write-full-word?' to always write 8 bytes
regardless of leading zeros. Such that an new-word=#xabcd will write #x0000000000abcd,
instead of just #abcd and leaving the leading bytes as they where."
  (with-open-file (str (get-mem-path pid) :element-type '(unsigned-byte 8) :direction :output
		       :if-exists :append)
    (file-position str address)
    (let ((byte-list (integer->byte-list new-word)))
      (when write-full-word?
	;; fill in byte-list with 0's, to match word-length
	(setf byte-list
	      (append byte-list
		      (make-list (- 8 (length byte-list)) :initial-element 0))))
      (n-write-proc-mem-bytes-list address byte-list :pid pid)))
  (when hex-print?
    (read-proc-mem-word address 0 pid)))

(defun rw-proc-mem-word (address &optional (offset 0) (rw-mode :r) new-word (pid *pid*))
  "Read or write word to memory. Using the `rw-mode' keyword switches :r = read, :w =
write, and :wf = write the full word.

Used to read through the memory interactively with the offset using the :r keyword, and
then when the memory address of interest was found, overwrite it by simply switching to
:w (write just the provided bytes) or :wf (always write the full word, with leading zeros
if needed) and provide the `new-word' to overwrite it."
  (if (or (eq rw-mode :w) (eq rw-mode :wf))
      (when (null new-word)
	(error "rw-mode is :w or :wf but no new-word to write
	provided. `new-word' is: ~a" new-word)))
  (case rw-mode
    (:r (read-proc-mem-word address offset t pid))
    (:w (write-proc-mem-word (+ address offset) new-word :pid pid))
    (:wf (write-proc-mem-word (+ address offset) new-word :write-full-word? t :pid pid))))


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

