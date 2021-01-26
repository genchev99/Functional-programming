#lang racket

(define (automorphic? n)
  (define (ends_equal? source ends_with)
    (define (last_number a)
      (remainder a 10)
    )
    
    (define (remove_last a)
      (quotient a 10)
    )
    
    (cond
      [(< source ends_with) #f]
      [(equal? ends_with 0) #t]
      [(not (equal? (last_number source) (last_number ends_with))) #f]
      [else (ends_equal? (remove_last source) (remove_last ends_with))]
    )
  )
  (ends_equal? (* n n) n)
)

(define (nth-cuban n)
  (define (helper_iter i counter)
    (define (is_prime? orig)
      (define (helper iter)
        (cond
          [(equal? iter 1) #t]
          [(equal? (remainder orig iter) 0) #f]
          [else (helper (sub1 iter))]
        )
      )
      (helper (sub1 orig))
    )
    (define (get_current_cuban) (- (* (add1 i) (add1 i) (add1 i)) (* i i i)))
    (define (is_cuban?) (is_prime? (get_current_cuban)))
    (cond
      [(is_cuban?) (cond
                     [(equal? (add1 counter) n) (get_current_cuban)]
                     [else (helper_iter (add1 i) (add1 counter))]
      )]
      [else (helper_iter (add1 i) counter)]
    )
  )
  (helper_iter 1 0)
)