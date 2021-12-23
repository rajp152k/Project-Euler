#lang racket

(require "utils.rkt")

;largest prime factor

(define largest-prime-factor
  (λ (num)
    (define largest-prime-factor-iter
      (λ (left largest-factor primes-left)
        (cond [(> (stream-first primes-left) left) largest-factor]
              [(divides? (stream-first primes-left) left) (largest-prime-factor-iter
                                                           (/ left (stream-first primes-left))
                                                           (max largest-factor (stream-first primes-left))
                                                           primes-left)]
              [#t (largest-prime-factor-iter left
                                             largest-factor
                                             (stream-rest primes-left))])))
    (largest-prime-factor-iter num 1 primes)))

(largest-prime-factor 600851475143)
;6857
