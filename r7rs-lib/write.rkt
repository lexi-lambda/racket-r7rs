#lang racket/base

(require (prefix-in 5: r5rs)
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out 5:display 5:write 7:write-shared)
         (rename-out [5:write write-simple]))

(define (7:write-shared v [out (current-output-port)])
  (parameterize ([print-graph #t])
    (5:write v out)))
