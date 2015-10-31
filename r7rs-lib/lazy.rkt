#lang racket/base

(require (prefix-in r: racket/promise)
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out r:delay delay-force r:force make-promise r:promise?))

(define-syntax-rule (delay-force expr)
  (r:delay (r:force expr)))

(define (make-promise x)
  (r:lazy x))
