#lang racket
(require "utils.rkt")

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

(foldl + 0 (sieve-till 2e6))
;142913828922
 
