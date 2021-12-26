#lang racket
(require "utils.rkt")

(define n-digit-nums
  (λ (n)
    (let [(high (sub1 (expt 10 n)))
          (low (expt 10 (sub1 n)))]
      (predicate-stepper-stream high
                                (λ (x)
                                  (>= x low))
                                sub1))))

(define num-palindrome?
  (λ (num)
    (palindrome? (number->string num))))

(define max-palindrome-prod
  (λ (primary-source secondary-source)
    (define primary-iter
      (λ (max-palindrome primary-source)
        
        (define secondary-iter
          (λ (primary secondary-stream)
            (if (stream-empty? secondary-stream)
                (primary-iter max-palindrome (stream-rest primary-source))
                (let [(curr-prod (* primary
                                    (stream-first secondary-stream) ))]
                  (cond [(<= curr-prod max-palindrome)
                         (primary-iter max-palindrome
                                       (stream-rest primary-source))]
                        [(num-palindrome? curr-prod)
                         (primary-iter curr-prod
                                       (stream-rest primary-source))]
                        [#t
                         (secondary-iter primary (stream-rest secondary-stream))])))))
        (if (stream-empty? primary-source)
            max-palindrome
            (secondary-iter (stream-first primary-source)
                            secondary-source))))
    (primary-iter 0 primary-source)))


(let [(3-digit-nums (n-digit-nums 1))]
  (max-palindrome-prod 3-digit-nums 3-digit-nums))
;906609

