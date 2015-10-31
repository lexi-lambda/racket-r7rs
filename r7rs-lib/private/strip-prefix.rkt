#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/parse)
         racket/provide)

(provide strip-colon-prefix-out)

(define-syntax strip-colon-prefix-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ spec ...)
        (expand-export #'(filtered-out (λ (name) (regexp-replace #px"^[^:]+:" name ""))
                                       (combine-out spec ...))
                       modes)]))))
