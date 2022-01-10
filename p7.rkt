#lang racket
(require "utils.rkt")

; no. of primes from 1 to n estimable as n/log(n).
; stream sieve too slow
; for 10,001st prime, choosing n as 1e6

(define sieve-till
  (λ (n)
    (let [(check-till (floor (sqrt n)))
          (base (range 2 (add1 n)))]
      (define siever
        (λ (left curr)
          (filter (λ (candidate)
                    (not (divides? curr candidate)))
                  left)))
      (define sieve-till-iter
        (λ (left)
          (cond [(> (car left) check-till) left]
                [#t (cons (car left)
                          (sieve-till-iter (siever (cdr left)
                                                   (car left)) ))])))
      (sieve-till-iter base))))

(list-ref (sieve-till 1e6) 10000)
;104743
