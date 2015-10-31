#lang racket/base

(require (prefix-in 6: rnrs/base-6)
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out 6:caaaar 6:caaadr 6:caaar 6:caadar 6:caaddr 6:caadr
                                 6:cadaar 6:cadadr 6:cadar 6:caddar 6:cadddr 6:caddr
                                 6:cdaaar 6:cdaadr 6:cdaar 6:cdadar 6:cdaddr 6:cdadr
                                 6:cddaar 6:cddadr 6:cddar 6:cdddar 6:cddddr 6:cdddr))
