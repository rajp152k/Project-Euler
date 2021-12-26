#lang racket

(provide divides?)
(provide integers-from)
(provide primes)
(provide predicate-stepper-stream)
(provide stream-splice)
(provide palindrome?)

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


(define predicate-stepper-stream
  (λ (initial valid? stepper)
  "returns a stream that repeatedly steps from a until the valid? predicate is true"
    (define stepper-iter 
      (λ (curr)
        (if (valid? curr)
            (stream-cons curr
                         (stepper-iter (stepper curr)))
            empty-stream)))
    (stepper-iter initial)))

(define stream-splice
  (λ (stream1 stream2)
    (cond [(stream-empty? stream1) stream2]
          [(stream-empty? stream2) stream1]
          [(stream-cons (stream-first stream1)
                        (stream-splice stream2
                                       (stream-rest stream1)))])))

(define palindrome?
  (λ (str)
    (let [(lis (string->list str))]
      (equal? (reverse lis) lis))))
