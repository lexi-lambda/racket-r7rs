#lang racket/base

(require compatibility/mlist
         racket/contract
         (prefix-in r: racket/base)
         (prefix-in 6: rnrs/lists-6))

(provide assoc list-copy list-set! make-list map member)

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

(define/contract (make-list n [v #f])
  ([exact-nonnegative-integer?] [any/c] . ->* . mlist?)
  (if (zero? n) null
      (mcons v (make-list (sub1 n) v))))

; R7RS’s map allows mapping over lists of different lengths, which differs from
; R5RS and R6RS’s implementation
(define map
  (case-lambda
    ; specialized for common case of a single list
    [(proc lst)
     (let loop ([lst lst])
       (if (null? lst) null
           (mcons (proc (mcar lst)) (loop (mcdr lst)))))]
    ; specialize for common case of a two lists
    [(proc lst-a lst-b)
     (let loop ([lst-a lst-a]
                [lst-b lst-b])
       (if (or (null? lst-a) (null? lst-b)) null
           (mcons (proc (mcar lst-a) (mcar lst-b))
                  (loop (mcdr lst-a) (mcdr lst-b)))))]
    ; other cases
    [(proc . lsts)
     (let loop ([lsts lsts])
       (if (ormap null? lsts) null
           (mcons (apply proc (r:map mcar lsts))
                  (loop (r:map mcdr lsts)))))]))

(define member
  (case-lambda
    [(el lst)    (6:member el lst)]
    [(el lst =?) (6:memp (λ (x) (=? el x)) lst)]))
