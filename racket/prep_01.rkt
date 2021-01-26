#lang racket

; 2 unit
; FIBONACI Iter
(define (my-fib n)
  (define (iterator left right crr)
    (cond
      [(>= crr (- n 1)) right]
      [else (iterator right (+ left right) (+ crr 1))]))
  (iterator 1 1 0))

(define (leap-year? year)
  (cond
    [(= 0 (modulo year 400)) #t]
    [(= 0 (modulo year 100)) #f]
    [(= 0 (modulo year 4)) #t]
    [else #f]))

(define (mymaxdivisor x)
  (define (helper d)
    (cond
      [(= (modulo x d) 0) d]
      [else (helper (- d 1))]))
  (helper (- x 1)))

(define (get-sum a b)
  (define (helper sum crr)
    (cond
      [(= (+ b 1) crr) sum]
      [(= 1 (modulo crr 2)) (helper (+ sum crr) (+ crr 1))]
      [else (helper sum (+ crr 1))]))
  (helper 0 a))

(define (prime? n)
  (define (helper d)
    (cond
      [(<= d 1) #t]
      [(= 0 (modulo n d)) #f]
      [else (helper (- d 1))]))
  (helper (- n 1)))

; 3 unit
(define (reverse-number n)
  (define (helper o r)
    (cond
      [(= o 0) r]
      [else (helper (quotient o 10) (+ (* 10 r) (modulo o 10)))]))
  (helper n 0))

; Задача 4. Да се дефинира функция, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число.
(define (delims n)
  (define (helper d c)
    (cond
      [(> d n) c]
      [(= 0 (modulo n d)) (helper (+ d 1) (+ c 1))]
      [else (helper (+ d 1) c)]))
  (helper 1 0))

; Задача 4. Да се дефинира функцията (perfect-number? n), която проверява дали числото n e съвършено, т.е. дали е равно на сбора на делителите си.

(define (perfect n)
  (define (get-sum-of-delims d c)
    (cond
      [(> d n) c]
      [(= 0 (modulo n d)) (get-sum-of-delims (+ d 1) (+ c d))]
      [else (get-sum-of-delims (+ d 1) c)]))
  (= (get-sum-of-delims 1 0) n))

; Задача 5. Да се дефинира функцията (inc-digits? n), която проверява дали цифрите на числто n са подредени в нарастващ ред.

(define (inc-digits? n)
  (define (helper l a)
    (cond
      [(= a 0) #t]
      [(< l (modulo a 10)) #f]
      [else (helper (modulo a 10) (quotient a 10))]))
  (helper (modulo n 10) (quotient n 10)))

; Задача 6. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
; Задача 7. Да се реши задача 6, чрез използване на не повече от n умножения.

(define (sum-seq x n)
  (define (iter sum b i)
    (cond
      [(>= i n) (+ sum b)]
      [else (iter (+ sum b) (* b x) (+ i 1))]))
  (iter 0 1 1))

; Зад. 2. Дефинирайте следните функции:
;
;  a). (my-identity x), функцията идентитет: връща каквото и дадете.
;
;  б). (my-compose f g), която връща композицията на функциите f и g.
;
;  в). (my-negate p?), която приема предикат p? и връща предиката (not p?).
;
;  г). (my-curry f x), която приема многоаргумента функция f и първи аргумент x и връща функцията получена от частичното прилагане на x върху f.

(define (my-identity x)
  x)

(define (my-compose f g)
  (lambda (x) (f (g x))))

(define (add1 x)
  (+ x 1))

(define (square x)
  (* x x))

(define f1 (my-compose square add1))

(define (my-negate p?)
  (λ (x) (not (p? x))))

((my-negate (λ (x) (= 0 (modulo x 2)))) 11)

; Зад. 3. Да се дефинира процедура от по-висок ред (difference F a b), която по дадени едноаргументна реална функция F и две реални числа a и b намира разликата F(b) - F(a).

; Да се оцени примерно орбъщение към процедурата.

(define (difference F a b)
  (- (F a) (F b)))

(define (f x) (* 2 x))
(λ (x) (* 2 x))

(let* ([x 5])
    (let ([x 2]
          [y x])
      (list y x)))
