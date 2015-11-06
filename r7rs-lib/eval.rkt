#lang racket/base

(require (prefix-in r: racket/base)
         "private/mutability.rkt")

(provide environment eval)

(define-namespace-anchor anchor)

(define (environment . import-specs)
  (let ([ns (namespace-anchor->empty-namespace anchor)])
    (parameterize ([current-namespace ns])
      (namespace-require 'r7rs)
      (r:eval `(import . ,(map to-immutable import-specs))))
    ns))

(define (eval expr env)
  (r:eval (to-immutable expr) env))
