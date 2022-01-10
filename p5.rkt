#lang racket
(require "utils.rkt")

(define range-lcm
  (λ (numbers)
    (define engulf
      (λ (consumer consumee)
        (define common (gcd consumer consumee))
        (* consumer (/ consumee common))))
    (define lcm-iter
      (λ (lcm-acc left)
        (cond [(empty? left) lcm-acc]
              [(divides? (car left) lcm-acc) (lcm-iter lcm-acc (cdr left))]
              [#t (lcm-iter (engulf lcm-acc (car left)) (cdr left))])))
    (lcm-iter 1 numbers)))

(range-lcm (range 1 21))
;232792560
