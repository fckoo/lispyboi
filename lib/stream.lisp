(provide "stream")

(defgeneric stream-putchar (stream character)
  "Write a single CHARACTER to STREAM")
(defgeneric stream-puts (stream string)
  "Write a single STRING to STREAM")
