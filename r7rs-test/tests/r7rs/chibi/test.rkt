#!r7rs

(define-library (chibi test)
  (export
   test test-equal test-error test-assert test-not test-values
   test-group current-test-group
   test-begin test-end test-syntax-error test-propagate-info
   test-vars test-run test-exit
   current-test-verbosity
   current-test-applier current-test-handler current-test-skipper
   current-test-group-reporter test-failure-count
   current-test-epsilon current-test-comparator)
  (import (except (scheme base) current-error-port)
          (scheme write)
          (scheme complex)
          (scheme process-context)
          (scheme time)
          (tests r7rs chibi term ansi))
  (cond-expand
   (chibi
    (import (only (chibi) pair-source print-exception protect)))
   (racket
    (import (only (racket base) error-display-handler current-error-port exn-message))
    (begin
      (define (pair-source x) #f)
      (define (print-exception ex out)
        (parameterize ((current-error-port out))
          ((error-display-handler) (exn-message ex) ex)))))
   (else
    (begin
      (define (pair-source x) #f)
      (define print-exception write))))
  (include "test.sps"))
