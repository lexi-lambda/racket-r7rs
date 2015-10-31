#lang racket/base

(require (prefix-in 6: rnrs/base-6)
         "private/strip-prefix.rkt")

(provide
 (strip-colon-prefix-out 6:angle 6:imag-part 6:magnitude 6:make-polar 6:make-rectangular 6:real-part))
