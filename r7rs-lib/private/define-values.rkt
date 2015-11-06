#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in 5: r5rs)
         (prefix-in r: racket/base))

(provide define-values)

(define-syntax (define-values stx)
  (define-syntax-class formals
    #:attributes [ids]
    (pattern id:id
             #:attr ids (list #'id))
    (pattern ()
             #:attr ids '())
    (pattern (id:id . rest:formals)
             #:attr ids (cons #'id (attribute rest.ids))))
  
  (syntax-parse stx
    [(_ formals:formals expr)
     #`(r:define-values #,(attribute formals.ids)
         (call-with-values (λ () expr)
                           ; use the R5RS lambda so that rest args are mlists, not regular lists
                           (5:lambda formals (values . #,(attribute formals.ids)))))]))
