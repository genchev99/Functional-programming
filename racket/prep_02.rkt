#lang racket

(define (sum-items xs)
  (cond [(null? xs) 0]
        [else (+ (car xs) (sum-items (cdr xs)))]))

(define (length xs)
  (cond [(null? xs) 0]
        [else (+ 1 (length (cdr xs)))]))

(define (contains x xs)
  (cond [(null? xs) #f]
        [(= (car xs) x) #t]
        [else (contains x (cdr xs))]))

(define (concat xs cs)
  (cond [(null? xs) cs]
        [else (cons (car xs) (concat (cdr xs) cs))]))

(define (reverse xs)
  (define (helper cs result)
    (cond [(null? cs) result]
          [else (helper (cdr cs) (cons (car cs) result))]))
  (helper xs `()))

(define (min-in-list xs)
  (define (helper min xs)
    (cond [(null? xs) min]
          [(< (car xs) min) (helper (car xs) (cdr xs))]
          [else (helper min (cdr xs))]))
  (helper (car xs) xs))

(define (max-in-list xs)
  (define (helper max xs)
    (cond [(null? xs) max]
          [(> (car xs) max) (helper (car xs) (cdr xs))]
          [else (helper max (cdr xs))]))
  (helper (car xs) xs))

(define (max-num-times xs)
  (define (helper max times xs)
    (cond [(null? xs) times]
          [(> (car xs) max) (helper (car xs) 1 (cdr xs))]
          [(= (car xs) max) (helper max (+ times 1) (cdr xs))]
          [else (helper max times (cdr xs))]))
  (helper (car xs) 0 xs))

(define (take xs n)
  (cond [(null? xs) `()]
        [(<= n 0) `()]
        [else (cons (car xs) (take (cdr xs) (- n 1)))]))

(define (drop xs n)
  (cond [(null? xs) `()]
        [(<= n 0) xs]
        [else (drop (cdr xs) (- n 1))]))

(define (zip xs cs)
  (cond [(null? xs) `()]
        [(null? cs) `()]
        [else (cons (cons (car xs) (car cs)) (zip (cdr xs) (cdr cs)))]))

(define (any? pred? xs)
  (cond [(null? xs) #f]
        [(pred? (car xs)) #t]
        [else (any? pred? (cdr xs))]))

(define (all? pred? xs)
  (cond [(null? xs) #t]
        [(not (pred? (car xs))) #f]
        [else (all? pred? (cdr xs))]))

(define (map f xs)
  (cond [(null? xs) `()]
        [else (cons (f (car xs)) (map f (cdr xs)))]))

(define (filter f xs)
  (cond [(null? xs) `()]
        [(f (car xs)) (cons (car xs) (filter f (cdr xs)))]
        [else (filter f (cdr xs))]))


(define (atom? x) (not (pair? x)))

(define (count-atoms xs)
  (cond [(null? xs) 0]
        [(atom? (car xs)) (+ 1 (count-atoms (cdr xs)))]
        [else (+ (count-atoms (car xs)) (count-atoms (cdr xs)))]))