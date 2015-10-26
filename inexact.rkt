#lang racket/base

(require (prefix-in 6: rnrs/base-6)
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out 6:acos 6:asin 6:atan 6:cos 6:exp 6:finite?
                                 6:infinite? 6:log 6:nan? 6:sin 6:sqrt 6:tan))
