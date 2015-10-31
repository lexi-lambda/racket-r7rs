#lang racket/base

(require (prefix-in 6: rnrs/eval-6)
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out 6:environment 6:eval))
