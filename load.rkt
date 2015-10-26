#lang racket/base

(require (prefix-in r: racket/base))

(provide load)

(define load
  (case-lambda
    [(path)
     (r:load path)]
    [(path environment)
     (parameterize ([current-namespace environment])
       (r:load path))]))
