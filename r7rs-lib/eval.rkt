#lang racket/base

(require (prefix-in r: racket/base))

(provide environment eval)

(define-namespace-anchor anchor)

(define (to-immutable p)
  (cond
    [(mpair? p) (cons (to-immutable (mcar p))
                      (to-immutable (mcdr p)))]
    [(vector? p)
     (let ([v (list->vector (map to-immutable (vector->list p)))])
       (if (immutable? p) (vector->immutable-vector v) v))]
    [else p]))

(define (environment . import-specs)
  (let ([ns (namespace-anchor->empty-namespace anchor)])
    (parameterize ([current-namespace ns])
      (namespace-require 'r7rs)
      (r:eval `(import . ,(map to-immutable import-specs))))
    ns))

(define (eval expr env)
  (r:eval (to-immutable expr) env))
