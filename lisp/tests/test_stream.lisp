(require "asserts")
(in-package :test-suite)

(with-input-from-string (is "hello world")
  (assert-eq 'hello (read is))
  (assert-eq 'world (read is))
  (assert-eq :eof (read is nil :eof)))

(with-input-from-string (is "#\\Newline #\\Space #\\Tab #\\Return")
  (assert-eql #\Newline (read is))
  (assert-eql #\Space (read is))
  (assert-eql #\Tab (read is))
  (assert-eql #\Return (read is))
  (assert-eq :eof (read is nil :eof)))

(with-input-from-string (is "  ")
  (assert-eq 'handled
             (handler-case (progn (read is)
                                  'not-handled)
               (end-of-file ()
                 'handled))))

(with-input-from-string (is "  ")
  (assert-eq 'eof (read is nil 'eof)))

(with-input-from-string (is "hello\nworld 123")
  (assert-eq 'hello (read is))
  (assert-eq 'world (read is))
  (assert-= '123 (read is))
  (assert-eq :eof (read is nil :eof)))

