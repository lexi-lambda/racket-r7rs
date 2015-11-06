#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/require
         (prefix-in 5: r5rs)
         (prefix-in 7: "base.rkt")
         (multi-in "private" ("export.rkt" "import.rkt" "library.rkt" "strip-prefix.rkt")))

(provide
 (all-from-out "private/export.rkt" "private/import.rkt")
 (strip-colon-prefix-out
  5:#%app 7:#%datum 5:#%expression 5:#%require 5:#%top 5:#%top-interaction define-library)
 (rename-out [module-begin #%module-begin]))

(define-syntax-rule (7:#%datum . datum)
  (7:quote datum))

(define-syntax (module-begin stx)
  (define-syntax-class non-library
    #:literals [define-library]
    (pattern (~not (define-library . _))))
  
  (syntax-parse stx
    #:literals [define-library]
    ; In the simplest case, no R7RS library is defined. Just expand to a Racket module.
    [(_ form:non-library ...)
     #'(5:#%module-begin form ...)]
    ; One R7RS library definition is allowed, but it is ignored and treated like `begin`.
    [(_ pre-form:non-library ...
        (define-library lib-form ...)
        post-form:non-library ...)
     #'(5:#%module-begin pre-form ... (define-library lib-form ...) post-form ...)]
    ; More than one library definition in a single Racket module is an error.
    [(_ _:non-library ...
        (define-library . _)
        _:non-library ...
        (~and second-lib (define-library . _))
        _ ...)
     (raise-syntax-error 'define-library
                         "More than one library definition in a single module is not permitted"
                         #'second-lib)]))
