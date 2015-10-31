#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "import.rkt")

(provide export)

(begin-for-syntax
  (define-syntax-class export-spec
    #:attributes [provide-spec]
    #:literals [rename]
    (pattern (rename id-orig:id id-new:id)
             #:with provide-spec #'(rename-out [id-orig id-new]))
    (pattern id:id
             #:with provide-spec #'id)))

(define-syntax export
  (syntax-parser
    [(_ spec:export-spec ...)
     #'(provide spec.provide-spec ...)]))
