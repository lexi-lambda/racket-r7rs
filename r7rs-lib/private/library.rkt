#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (only-in "../base.rkt" include)
         "cond-expand.rkt"
         "export.rkt"
         "import.rkt")

(provide (all-from-out "import.rkt" "export.rkt")
         define-library begin cond-expand include)

(begin-for-syntax
  (define-syntax-class library-clause
    #:description "library clause"
    #:attributes []
    #:commit
    (pattern ({~or* {~literal import}
                    {~literal export}
                    {~literal begin}
                    {~literal cond-expand}
                    {~literal include}}
              . rest))))

(define-syntax (define-library stx)
  (unless (eq? (syntax-local-context) 'module)
    (raise-syntax-error 'define-library "only allowed in a module context" stx))
  (syntax-parse stx
    [(_ _ clause:library-clause ...)
     #'(begin clause ...)]))
