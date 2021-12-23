#lang racket

(provide divides?)
(provide integers-from)
(provide primes)

(define divides?
  (λ (a b)
    "returns whether a divides b"
    (= (remainder b a)
       0)))


(define integers-from
  (λ (initial)
    "returns a stream of integers starting from initial"
    (stream-cons initial
                 (integers-from (+ 1 initial)))))

(define sieve
  (λ (seq)
    (stream-cons (stream-first seq)
                 (stream-filter (λ (x)
                                  (not (divides? (stream-first seq) x)))
                                (stream-rest seq)))))

(define primes
  (begin
    "returns a stream of primes"
    (sieve (integers-from 2))))
