(in-package :lispyboi)
(provide "string")


(defun string-match (string index match-string)
  (when (<= (+ index (length match-string)) (length string))
    (let ((i 0))
      (while (and (< i (length match-string))
                  (eql (aref string (+ index i)) (aref match-string i)))
             (incf i))
      (= i (length match-string)))))


(defun delimiter-p (string index delimiter)
  (typecase delimiter
    (character (and (< index (length string))
                    (eql delimiter (aref string index))))
    (string (string-match string index delimiter))))

(defun string-split (string &optional (delimiter #\Space) max-splits)
  "Splits STRING on a given DELIMITER up to a maximum of MAX-SPLITS.

MAX-SPLITS may be NIL or a negative FIXNUM to split on every occurence of
DELIMITER or a positive value representing the amount of splits.

e.g.: if MAX-SPLITS is 1, a list containing up to two strings is returned.
      if MAX-SPLITS is 5, a list containing up to six strings is returned.
      etc."
  (let ((splits)
        (start 0)
        (end 0)
        (delim-length (typecase delimiter
                        (character 1)
                        (string (length delimiter)))))
    (unless max-splits
      (setf max-splits -1))
    (while (and (/= 0 max-splits)
                (< end (length string)))
           (when (delimiter-p string end delimiter)
             (push (substring string start end) splits)
             (setf start (+ delim-length end))
             (decf max-splits))
           (incf end))
    (when (< start end)
      (push (substring string start end) splits))
    (when (and (= max-splits 0)
               (< end (length string)))
      (push (substring string (+ end (- delim-length 1))) splits))
    (reverse! splits)))

(defun string-trim (string)
  "Returns a copy of STRING with both leading and trailing whitespace characters
removed."
  (let ((start 0)
        (end (length string)))
    (while (and (< start end)
                (spacep (aref string start)))
           (incf start))
    (while (and (> end start)
                (spacep (aref string (- end 1))))
           (decf end))
    (if (= start end)
        (make-string)
        (substring string start end))))

(defun string-trim-left (string)
  "Returns a copy of STRING with leading whitespace characters removed."
  (let ((start 0)
        (end (length string)))
    (while (and (< start end)
                (spacep (aref string start)))
           (incf start))
    (substring string start)))

(defun string-trim-right (string)
  "Returns a copy of STRING with trailing whitespace characters removed."
  (let ((end (length string)))
    (while (and (> end 0)
                (spacep (aref string (- end 1))))
           (decf end))
    (substring string 0 end)))

(defun array-join (separator &rest arrays)
  "Returns an new array containing every array in ARRAYS separated by SEPARATOR.

SEPARATOR will be copied up to a maximum of (- (length arrays) 1) times.

e.g: (array-join \", \" \"1\" \"2\" \"3\" \"4\") ==> \"1, 2, 3, 4\" "
  (let* ((lengths (map #'length arrays))
         (total-length (+ (* (length separator) (- (length arrays) 1))
                          (apply #'+ lengths)))
         (new-array (make-array total-length (array-type (first arrays))))
         (new-array-idx 0))
    (dotimes (i (length (car arrays)))
      (setf (aref new-array new-array-idx) (aref (car arrays) i))
      (incf new-array-idx))
    (dolist (array (cdr arrays))
      (dotimes (i (length separator))
        (setf (aref new-array new-array-idx) (aref separator i))
        (incf new-array-idx))
      (dotimes (i (length array))
        (setf (aref new-array new-array-idx) (aref array i))
        (incf new-array-idx)))
    new-array))

(defun char-downcase (c)
  (if (<= (char-code #\A) (char-code c) (char-code #\Z))
      (code-char (bit-ior (char-code c) 32))
      c))

(defun char-upcase (c)
  (if (<= (char-code #\a) (char-code c) (char-code #\z))
      (code-char (bit-xor (char-code c) 32))   
      c))

(defun string-upcase! (string)
  "Modifies STRING, changing all ASCII characters to their uppercase counterparts."
  (dotimes (i (length string))
    (when (<= 0 (char-code (aref string i)) 127)
      (setf (aref string i) (char-upcase (aref string i)))))
  string)

(defun string-downcase! (string)
  "Modifies STRING, changing all ASCII characters to their lowercase counterparts."
  (dotimes (i (length string))
    (when (<= 0 (char-code (aref string i)) 127)
      (setf (aref string i) (char-downcase (aref string i)))))
  string)

(export '(string-split
          string-trim
          string-trim-left
          string-trim-right
          array-join
          char-downcase
          char-upcase
          string-upcase!
          string-downcase!))
