#lang racket

(require 2htdp/batch-io)

(define (main)
  (apply + (take (sort (cals (drop-right (read-lines "calories.txt") 1)) >) 3)))

(define (cals lines)
  (if (index-of lines "")
      (let ([my-split (list (take lines (index-of lines "")) (drop lines (+ 1 (index-of lines ""))))])
        (cons (apply + (map string->number (first my-split)))
              (cals (second my-split))))
      (cons (apply + (map string->number lines)) empty)))

(define (max-ls lst) (cond
                       [(empty? lst) (empty)]
                       [(empty? (rest lst)) (first lst)]
                       [(< (first lst) (first (rest lst))) (max-ls (rest lst))]
                       [else (max-ls (cons (first lst) (rest (rest lst))))]))

(main)

