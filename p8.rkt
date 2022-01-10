#lang racket

(define duplicate-and-shift-base-list
  (λ (base-lists base-list duplications-left)
    (if (= duplications-left 1)
        (cons base-list base-lists)
        (duplicate-and-shift-base-list (cons base-list base-lists)
                                       (cdr base-list)
                                       (sub1 duplications-left)))))

(define truncate-shifted-duplications
  (λ (shifted-duplications)
    (let [(base-len (length (car shifted-duplications)))]
      (map (λ (shifted-duplication) (take shifted-duplication base-len))
           shifted-duplications))))

(define gen-prods
  (λ (input-num-list conseq-prod-int)
    (apply map (cons * (truncate-shifted-duplications (duplicate-and-shift-base-list '()
                                                                                     input-num-list
                                                                                     conseq-prod-int))))))

(define num->list
  (λ (num)
    (map (compose string->number string)
         (string->list (number->string num)))))

(define sol
  (λ ()
    (define input-num (read))
    (define conseq (read))
    (apply max (gen-prods (num->list input-num) conseq))))

(sol)
