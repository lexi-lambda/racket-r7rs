#lang racket/base

;; This module provides variants of RackUnit checks that are reimplemented to
;; use R7RS’s version of `quote` to preserve expressions in error messages. This
;; is necessary because R7RS expressions can contain circular data;
;; see Note [The graph structure story] in r7rs/lang/reader for more details.

(require (for-syntax racket/base
                     racket/syntax
                     syntax/transformer)
         rackunit
         syntax/parse/define
         (prefix-in 7: r7rs/base))

(provide (rename-out [7:check-equal? check-equal?]
                     [7:check-exn check-exn]
                     [7:convert-syntax-error convert-syntax-error]))

;; -----------------------------------------------------------------------------

(define-for-syntax (make-check-transformer proc-id)
  (syntax-parser
    [(name . args)
     #`(with-default-check-info*
           (list (make-check-name (7:quote name))
                 (make-check-location '#,(list (syntax-source this-syntax)
                                               (syntax-line this-syntax)
                                               (syntax-column this-syntax)
                                               (syntax-position this-syntax)
                                               (syntax-span this-syntax)))
                 (make-check-expression (7:quote (name . args))))
         #,(quasisyntax/loc this-syntax
             (λ ()
               ((current-check-around)
                #,(quasisyntax/loc this-syntax
                    (λ ()
                      (#,proc-id . args)))))))]))

(define (build-check-info #:params params
                          #:message message)
  (define base-info (list (make-check-params params)))
  (if message
      (cons (make-check-message message) base-info)
      base-info))

(define-syntax-parser define-check/r7rs
  [(_ (name:id param:id ...) body ...+)
   #`(begin
       (define check-proc
         #,(quasisyntax/loc this-syntax
             (λ (param ... [message #f])
               (with-default-check-info* (build-check-info #:params (list param ...)
                                                           #:message message)
                 #,(syntax/loc this-syntax
                     (λ () body ...))))))
       (define-syntax name (make-check-transformer (quote-syntax check-proc))))])

(define-syntax-parser define-binary-check/r7rs
  [(_ (name:id pred:expr))
   (syntax/loc this-syntax
     (define-check/r7rs (name actual expected)
       (with-default-check-info*
           (list (make-check-actual actual)
                 (make-check-expected expected))
         (λ ()
           (or (pred actual expected)
               (fail-check))))))])

(define-binary-check/r7rs (7:check-equal? 7:equal?))

(define-check/r7rs (7:check-exn raw-pred thunk)
  (define pred (cond
                 [(regexp? raw-pred)
                  (λ (x) (and (exn:fail? x) (regexp-match raw-pred (exn-message x))))]
                 [(and (procedure? raw-pred) (procedure-arity-includes? raw-pred 1))
                  raw-pred]
                 [else
                  (raise-argument-error 'check-exn "(or/c (-> any/c any/c) regexp?)" raw-pred)]))
  (let/ec succeed
    (with-handlers
        ([pred (lambda (exn) (succeed (void)))]
         ;; catch any other exception and raise an check
         ;; failure
         [(λ (exn)
            (and (exn:fail? exn)
                 (not (exn:test? exn))))
          (lambda (exn)
            (with-default-check-info*
                (list (make-check-message "Wrong exception raised")
                      (make-check-info 'exn-message (exn-message exn))
                      (make-check-info 'exn exn))
              fail-check))])
      (thunk))
    (with-default-check-info*
        (list (make-check-message "No exception raised"))
      fail-check)))

;; -----------------------------------------------------------------------------

(define-for-syntax (syntax->expression stx)
  (define placeholders (make-hash))

  (define expr
    (let loop ([stx stx])
      (define here-stx (and (syntax? stx)
                            #`(quote-syntax #,(datum->syntax stx #f stx stx))))
      (syntax-parse stx
        [(a . b)
         #`(datum->syntax #f
                          (cons #,(loop #'a) #,(loop #'b))
                          #,here-stx
                          #,here-stx)]
        [#(e ...)
         #`(datum->syntax #f
                          (vector-immutable #,@(map loop (attribute e)))
                          #,here-stx
                          #,here-stx)]
        [ph-stx
         #:do [(define ph (syntax-e #'ph-stx))]
         #:when (placeholder? ph)
         (define ph-id
           (cond
             [(hash-ref placeholders ph #f) => car]
             [else
              (define ph-id (generate-temporary #'ph-stx))
              (hash-set! placeholders ph (list ph-id #f))
              (define ph-e (loop (placeholder-get ph)))
              (hash-set! placeholders ph (list ph-id ph-e))
              ph-id]))
         #`(datum->syntax #f #,ph-id #,here-stx #,here-stx)]
        [_
         #`(quote-syntax #,this-syntax)])))

  (define/syntax-parse ([ph-id ph-e] ...) (hash-values placeholders))
  #`(let ([ph-id (make-placeholder #f)] ...)
      (placeholder-set! ph-id ph-e) ...
      #,expr))

(define-for-syntax (reraise-syntax-expression exn)
  #`(raise (exn:fail:syntax
            #,(exn-message exn)
            (current-continuation-marks)
            (list #,@(map syntax->expression (exn:fail:syntax-exprs exn))))))

(define-syntax 7:convert-syntax-error
  (make-expression-transformer
   (syntax-parser
     [(_ e:expr)
      (parameterize ([error-print-source-location #f])
        (with-handlers ([exn:fail:syntax? reraise-syntax-expression])
          (local-expand #'e 'expression null)))])))
