(provide "stream")

(defgeneric stream-putchar (stream character)
  "Write a single CHARACTER to STREAM.")

(defgeneric stream-puts (stream string)
  "Write a single STRING to STREAM.")

(defgeneric stream-eof-p (stream)
  "Returns T when STREAM reaches the end, otherwise NIL.")

(defgeneric stream-peekc (stream eof-error-p eof-value)
  "Returns the next character in STREAM without consuming it.")

(defgeneric stream-getc (stream eof-error-p eof-value)
  "Consumes and returns the next character in STREAM.")
