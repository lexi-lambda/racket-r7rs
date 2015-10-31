#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide define-library)

(define-syntax (define-library stx)
  (unless (eq? (syntax-local-context) 'module)
    (raise-syntax-error 'define-library "not in module context" stx))
  (syntax-parse stx
    [(_ _ body ...)
     #'(begin body ...)]))
