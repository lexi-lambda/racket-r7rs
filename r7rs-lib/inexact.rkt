#lang racket/base

(require racket/contract
         (prefix-in 6: rnrs/base-6)
         (prefix-in r: racket/math)
         "private/strip-prefix.rkt")

(provide infinite? finite? nan?
         (strip-colon-prefix-out 6:acos 6:asin 6:atan 6:cos 6:exp 6:log 6:sin 6:sqrt 6:tan))

(define/contract (infinite? x)
  (number? . -> . boolean?)
  (or (r:infinite? (real-part x))
      (r:infinite? (imag-part x))))

(define/contract (finite? x)
  (number? . -> . boolean?)
  (not (infinite? x)))

(define/contract (nan? x)
  (number? . -> . boolean?)
  (or (r:nan? (real-part x))
      (r:nan? (imag-part x))))
