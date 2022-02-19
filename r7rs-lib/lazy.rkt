#lang racket/base

;; This implementation of promises is necessarily incompatible with
;; racket/promise because R7RS allows `force` to be called on promises that are
;; currently running. It is partially based on the sample implementation given
;; in section 7.3 of the R7RS specification.

(provide delay delay-force force make-promise promise?)

(struct promise
  (done?  ; boolean?
   value) ; (if done? any/c (-> promise?))
  #:mutable)

(define-syntax-rule (delay expr)
  (delay-force (promise #t expr)))

(define-syntax-rule (delay-force expr)
  (promise #f (λ () expr)))

(define (force p)
  (if (promise? p)
      (if (promise-done? p)
          (promise-value p)
          (let ([v ((promise-value p))])
            (unless (promise-done? p)
              (set-promise-value! p (promise-value v))
              (set-promise-done?! p (promise-done? v)))
            (force p)))
      p))

(define (make-promise v)
  (if (promise? v) v (delay v)))
