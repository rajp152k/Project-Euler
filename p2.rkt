#lang racket

; acc even fibs < 4e6

(define fib-acc
  (λ (limit validator)
    (define fib-iter
      (λ (a b acc)
        (if (< b limit)
            (if (validator b)
                (fib-iter b (+ a b) (+ b acc))
                (fib-iter b (+ a b) acc))
            acc)))
      (fib-iter 0 1 0)))

(fib-acc 4e6 even?)

;4613732
