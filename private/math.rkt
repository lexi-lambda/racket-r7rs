#lang racket/base

(require racket/contract)

(provide floor/ floor-quotient floor-remainder
         (rename-out [quotient/remainder truncate/]
                     [quotient truncate-quotient]
                     [remainder truncate-remainder]))

(define/contract (floor/ a b)
  (integer? integer? . -> . (values integer? integer?))
  (let* ([q (floor (/ a b))]
         [r (- a (* b q))])
    (values q r)))

(define/contract (floor-quotient a b)
  (integer? integer? . -> . integer?)
  (floor (/ a b)))

(define (floor-remainder a b)
  (integer? integer? . -> . integer?)
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
