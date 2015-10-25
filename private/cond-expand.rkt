#lang racket/base

; This implementation of cond-expand is a fake stub: it always takes the else branch, and it ignores
; everything else. Eventually it will need to be implemented.

(require (prefix-in 5: r5rs))

(provide cond-expand features)

(define (features)
  (5:list))

(define-syntax ce-clause
  (syntax-rules (5:else)
    [(_ 5:else . exprs) (begin . exprs)]
    [(_ . _) (begin)]))

(define-syntax-rule (cond-expand clause0 clause ...)
  (begin (ce-clause . clause0) (ce-clause . clause) ...))
