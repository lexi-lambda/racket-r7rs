#lang racket/base

(require (prefix-in 5: r5rs)
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out 5:read))
