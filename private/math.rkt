#lang racket/base

(provide floor/ floor-quotient floor-remainder
         (rename-out [quotient/remainder truncate/]
                     [quotient truncate-quotient]
                     [remainder truncate-remainder]))

(define (floor/ a b)
  (let* ([q (floor (/ a b))]
         [r (- a (* b q))])
    (values q r)))

(define (floor-quotient a b)
  (floor (/ a b)))

(define (floor-remainder a b)
  (let-values ([(q r) (floor/ a b)])
    r))

(module+ test
  (require rackunit (submod ".."))

  (test-case
   "rounding division"
   (let-values ([(q r) (floor/ 5 2)])
     (check-eqv? q 2)
     (check-eqv? r 1))
   (let-values ([(q r) (floor/ -5 2)])
     (check-eqv? q -3)
     (check-eqv? r 1))
   (let-values ([(q r) (floor/ 5 -2)])
     (check-eqv? q -3)
     (check-eqv? r -1))
   (let-values ([(q r) (floor/ -5 -2)])
     (check-eqv? q 2)
     (check-eqv? r -1)))
  
  (test-case
   "truncating division"
   (let-values ([(q r) (truncate/ 5 2)])
     (check-eqv? q 2)
     (check-eqv? r 1))
   (let-values ([(q r) (truncate/ -5 2)])
     (check-eqv? q -2)
     (check-eqv? r -1))
   (let-values ([(q r) (truncate/ 5 -2)])
     (check-eqv? q -2)
     (check-eqv? r 1))
   (let-values ([(q r) (truncate/ -5 -2)])
     (check-eqv? q 2)
     (check-eqv? r -1))
   (let-values ([(q r) (truncate/ -5.0 -2)])
     (check-eqv? q 2.0)
     (check-eqv? r -1.0))))
