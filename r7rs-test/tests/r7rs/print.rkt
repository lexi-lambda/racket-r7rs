#lang r7rs

(import (only (racket base) module))

(module test racket/base
  (require (for-syntax racket/base)
           racket/runtime-path version/utils)
  (define-runtime-module-path-index path '(submod ".."))
  ; `raco test` doesn’t seem to work quite right with respect to configure-runtime submodules on
  ; versions prior to 6.4, so skip this test on older versions of Racket
  (when (version<=? "6.4" (version))
    (dynamic-require path #f)))

(import (scheme base)
        (rackunit)
        (only (racket base) current-print)
        (only (racket port) with-output-to-string))

(check-equal? (with-output-to-string
                (lambda () ((current-print) (list 1 2 3))))
              "(1 2 3)\n")

(check-equal? (with-output-to-string
                (lambda () ((current-print) (cons 1 2))))
              "(1 . 2)\n")
