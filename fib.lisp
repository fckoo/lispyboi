(setq fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
(dump-env)
(fib 10)

