#lang racket/base

(require compatibility/mlist
         racket/contract
         (prefix-in 6: rnrs/lists-6))

(provide assoc list-copy list-set! make-list member)

(define assoc
  (case-lambda
    ([el alst]    (6:assoc el alst))
    ([el alst =?] (6:assp (λ (x) (=? el x)) alst))))

; R7RS defines list-copy in such a way so that it may operate on improper lists
(define (list-copy v)
  (if (mpair? v)
      (mcons (mcar v) (list-copy (mcdr v)))
      v))

(define/contract (list-set! lst n v)
  (mlist? exact-nonnegative-integer? any/c . -> . void?)
  (let loop ([lst lst]
             [n n])
    (if (zero? n)
        (set-mcar! lst v)
        (loop (mcdr lst) (sub1 n)))))

(define/contract (make-list n v)
  (exact-nonnegative-integer? any/c . -> . mlist?)
  (if (zero? n) null
      (mcons v (make-list (sub1 n) v))))

(define member
  (case-lambda
    [(el lst)    (6:member el lst)]
    [(el lst =?) (6:memp (λ (x) (=? el x)) lst)]))
