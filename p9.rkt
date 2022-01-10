#lang racket

(define enumerate-pythogorean-triplets
  (λ (from till)
    (define primary-iter
      (λ (primary-head primary-accd)
        (if (> primary-head till)
            primary-accd
            (let [(increment (foldl (λ (triples more-triples)
                                      (append triples more-triples))
                                    '()
                                    (map (λ (double) (list (cons primary-head double) ))
                                         (secondary-iter primary-head
                                                         (add1 primary-head)
                                                         '()))))]
              (primary-iter (add1 primary-head)
                            (append primary-accd increment))))))
    (define secondary-iter
      (λ (primary-head secondary-head secondary-accd)
        (if (> secondary-head till)
            secondary-accd
            (let* [(tertiary-candidates (filter (λ (tertiary-candidate) (= (+ (sqr primary-head)
                                                                              (sqr secondary-head))
                                                                           (sqr tertiary-candidate)))
                                                (range (add1 secondary-head)
                                                       (add1 till))))
                   (increment (map (λ (tertiary-candidate) (list secondary-head tertiary-candidate))
                                   tertiary-candidates))]
              (secondary-iter primary-head
                              (add1 secondary-head)
                              (append secondary-accd increment))))))
    (primary-iter from '())))

(define sol
  (λ ()
    (apply * (car (filter (λ (triplet)
                      (= (apply + triplet) 1000))
                    (enumerate-pythogorean-triplets 1 1000)) ))))

