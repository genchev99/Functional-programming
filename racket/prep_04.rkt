#lang racket

; task1 . Да се дефинира функция (sum-numbers a b), приемаща два аргумента, която намира сумата на числата в интервала [a,b], чиито цифри са в низходящ (>=) ред

(define (last-digit n) (remainder n 10))
(define (asc? n)
  (define next (quotient n 10))
  (cond [(< n 10)                             #t]
        [(> (last-digit n) (last-digit next)) #f]
        [else                                 (asc? next)]))

(define (sum-numbers a b)
  (define (iter res crr)
    (cond [(> crr b) res]
          [(asc? crr) (iter (+ res crr) (+ 1 crr))]
          [else (iter res (+ 1 crr))]))
  (iter 0 a))

(define (num-bigger-elements lst)
  (define (bigger-counter num counter lst)
    (cond [(null? lst) counter]
          [(> (car lst) num) (bigger-counter num (+ 1 counter) (cdr lst))]
          [else (bigger-counter num counter (cdr lst))]))
  (define (iter res xs)
    (cond [(null? xs) (reverse res)]
          [else (iter (cons  (list (car xs) (bigger-counter (car xs) 0 lst)) res) (cdr xs))]))
  (iter `() lst))


; Ако f и g са числови функции и n е естествено число, да се дефинира функция от повисок ред (switchsum f g n), която връща като резултат функция, чиято стойност в дадена
; точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).

(define (switchsum f g n)
  (define (helper res prev count x)
    (cond [(= count n) res]
          [(odd? count) (helper (+ res prev) (f prev) (+ 1 count) x)]
          [else (helper (+ res prev) (g prev) (+ 1 count) x)]))
  (λ (x) (helper 0 (f x) 0 x)))

(define (join times string glue)
  (define (helper res crr)
    (cond [(= (+ crr 1) times) (string-append res string)]
          [else (helper (string-append res string glue) (+ 1 crr))]))
  (helper "" 0))

(define (repeater str)
  (λ (count glue) (join count str glue)))

; Задача 5. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на
; естествените числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k.

(define (sum-sum-digit a b k)
  (define (get-sum num)
    (define (helper res num)
      (cond [(<= num 0) res]
            [else (helper (+ res (modulo num 10)) (quotient num 10))]))
    (helper 0 num))
  
  (define (del? num)
    (= 0 (modulo (get-sum num) k)))
  
  (define (iter res crr)
    (cond [(> crr b) res]
          [(del? crr) (iter (+ res crr) (+ 1 crr))]
          [else (iter res (+ 1 crr))]))
  (iter 0 a))

(define (max-ordered-sublist ll)
  (define (helper m prev new ll)
    (cond [(null? ll) m]
          [(> (car ll) prev) (helper (max m (+ 1 new)) (car ll) (+ 1 new) (cdr ll))]
          [else (helper (max m new) (car ll) 1 (cdr ll))]))
  (helper 1 (car ll) 1 (cdr ll)))

(define (set-union xs ys)
  (define (helper xs ys result)
    (cond [(and (empty? xs) (empty? ys)) result]
          [(empty? xs) (append result ys)]
          [(empty? ys) (append result xs)]
          [(< (car xs) (car ys)) (helper (cdr xs) ys (append result (list(car xs))))]
          [(> (car xs) (car ys)) (helper xs (cdr ys) (append result (list(car ys))))]
          [else (helper (cdr xs) (cdr ys) (append result (list(car xs))))]))
  (helper xs ys '()))
