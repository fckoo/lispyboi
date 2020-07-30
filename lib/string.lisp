(in-package :lispyboi)
(provide "string")

(defun string-split (string &optional (delimiter #\Space) max-splits)
  (let ((splits)
        (start 0)
        (end 0))
    (unless max-splits
      (setf max-splits -1))
    (while (and (/= 0 max-splits)
                (< end (length string)))
           (when (eql delimiter (aref string end))
             (push (substring string start end) splits)
             (setf start (+ 1 end))
             (decf max-splits))
           (incf end))
    (when (< start end)
      (push (substring string start end) splits))
    (when (and (= max-splits 0)
               (< end (length string)))
      (push (substring string end) splits))
    (reverse splits)))

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
          char-downcase
          char-upcase
          string-upcase!
          string-downcase!))
