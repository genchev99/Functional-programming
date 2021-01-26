#lang racket

(define (mymin x y) (if (< x y) x y))

(define (inside x a b) (if (and (>= x a) (<= x b)) #t #f))

(define (myfunc x y) (/ (+ (* x x) (* y y)) 2))

(define (myfib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (myfib (- n 1)) (myfib (- n 2))))
  )
)

(define (mygcd x y)
  (cond
    ((< x y) (mygcd x (- y x)))
    ((> x y) (mygcd (- x y) y))
    (else x))
)
