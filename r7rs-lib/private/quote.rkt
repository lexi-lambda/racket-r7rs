#lang racket/base

;; This module provides versions of `quote` and `#%datum` that produce mutable
;; pairs and handle graph structure appropriately. For more information on the
;; latter, see Note [The graph structure story] in r7rs/lang/reader.

(require (for-syntax racket/base
                     racket/syntax)
         racket/shared
         syntax/parse/define)

(provide (rename-out [7:quote quote]
                     [7:#%datum #%datum]))

;; -----------------------------------------------------------------------------

(define-for-syntax (do-quote form
                             #:context context
                             #:allow-placeholders? allow-placeholders?)
  (define any-pairs? #f)
  (define placeholders (make-hasheq))

  (define expr
    (let loop ([form form])
      (syntax-parse form
        #:context context
        [(a . b)
         (set! any-pairs? #t)
         (quasisyntax/loc this-syntax
           (mcons #,(loop #'a) #,(loop #'b)))]
        [#(e ...)
         (quasisyntax/loc this-syntax
           (vector-immutable #,@(map loop (attribute e))))]
        [ph-stx
         #:do [(define ph (syntax-e #'ph-stx))]
         #:when (placeholder? ph)
         (unless allow-placeholders?
           (raise-syntax-error #f "unquoted datum label" context #'ph-stx))

         (define ph-id
           (cond
             [(hash-ref placeholders ph #f) => car]
             [else
              (define ph-id (generate-temporary #'ph-stx))
              (hash-set! placeholders ph (list ph-id #f))
              (define ph-e (loop (placeholder-get ph)))
              (hash-set! placeholders ph (list ph-id ph-e))
              ph-id]))
         (datum->syntax ph-id (syntax-e ph-id) #'ph-stx)]
        [other
         (syntax/loc this-syntax
           (quote other))])))

  (cond
    [(not (hash-empty? placeholders))
     (syntax-local-lift-expression
      (quasisyntax/loc context
        (shared #,(hash-values placeholders)
          #,expr)))]
    [any-pairs?
     (syntax-local-lift-expression expr)]
    [else
     (quasisyntax/loc context
       (quote #,form))]))

(define-syntax-parser 7:quote
  [(_ form)
   (do-quote #'form
             #:context this-syntax
             #:allow-placeholders? #t)])

(define-syntax-parser 7:#%datum
  [(_ . form)
   (do-quote #'form
             #:context this-syntax
             #:allow-placeholders? #f)])
