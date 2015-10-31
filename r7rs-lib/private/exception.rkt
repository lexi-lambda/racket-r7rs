#lang racket/base

(require racket/contract
         racket/string)

(provide error
         (rename-out [exn:fail:r7rs? error-object?]
                     [exn:fail:r7rs-message error-object-message]
                     [exn:fail:r7rs-irritants error-object-irritants]))

(struct exn:fail:r7rs exn:fail (message irritants)
  #:reflection-name 'error-object)

(define/contract (error message . irritants)
  ([string?] [] #:rest list? . ->* . any)
  (raise (exn:fail:r7rs (format "~a\n irritants:\n~a" message
                                (string-join (map (λ (x) (format "  ~e" x)) irritants) "\n"))
                        (current-continuation-marks)
                        message
                        irritants)))
