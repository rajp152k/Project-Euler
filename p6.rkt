#lang racket

(define square
  (λ (x) (* x x)))

(define sum
  (λ (lis)
    (foldl + 0 lis)))

(define sum-squares
  (λ (lis)
    (foldl + 0
           (map square lis))))

(define square-sum
  (λ (lis)
    (square (sum lis))))

(define sqr-sum-less-sum-sqr
  (λ (lis)
    (- (square-sum lis)
       (sum-squares lis))))

(sqr-sum-less-sum-sqr (range 1 101))
;25164150



