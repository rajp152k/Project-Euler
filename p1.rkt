#lang racket

;multiples of 3 and 5 below 1000

(stream-fold + 0 (stream-filter (λ (x)
                            (or (= (remainder x 3) 0)
                                (= (remainder x 5) 0)))
                          (in-range 0 1000 1)))

;233168
