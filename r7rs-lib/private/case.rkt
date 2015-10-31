#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide case)

(define-syntax (case stx)
  (define-syntax-class case-clause
    #:attributes [cond-clause]
    #:literals [=>]
    (pattern [(datum ...) => fn]
             #:attr cond-clause
             (λ (in) #`[(or (eqv? #,in 'datum) ...)
                        (fn #,in)]))
    (pattern [(datum ...) expr ...+]
             #:attr cond-clause
             (λ (in) #`[(or (eqv? #,in 'datum) ...)
                        expr ...])))

  (define-syntax-class else-clause
    #:attributes [cond-clause]
    #:literals [=> else]
    (pattern [else => fn]
             #:attr cond-clause
             (λ (in) #`[else (fn #,in)]))
    (pattern [else expr ...+]
             #:attr cond-clause
             (λ (in) #`[else expr ...])))

  (define-syntax-class case-or-else-clause
    #:attributes [cond-clause]
    (pattern clause:case-clause #:attr cond-clause (attribute clause.cond-clause))
    (pattern clause:else-clause #:attr cond-clause (attribute clause.cond-clause)))
  
  (syntax-parse stx
    [(_ key clause:case-clause ... final:case-or-else-clause)
     #`(let ([v key])
         (cond
           #,@(map (λ (c) (c #'v)) (attribute clause.cond-clause))
           #,((attribute final.cond-clause) #'v)))]))
