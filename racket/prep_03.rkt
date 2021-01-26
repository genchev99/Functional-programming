#lang racket

(define matrix `((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; Takes matrix, returns matrix
;(define (trans m)
;  (define (reverse-each-row x)
;    (cond [(null? x) `()]
;          [else (cons (reverse (car x)) (reverse-each-row (cdr x)))]))
;  (reverse-each-row m))

(define (trans m)
  (define (helper res it copy)
    (cond [(null? copy) res]
          [else (helper (append res (get-nth-col it `() m)) (+ it 1) (cdr copy))]))
  (define (get-nth-col n r m)
    (cond [(null? m) (list (reverse r))]
          [else (get-nth-col n (append (list (list-ref (car m) n)) r) (cdr m))]))
  (helper `() 0 (car m)))

(trans matrix)
matrix