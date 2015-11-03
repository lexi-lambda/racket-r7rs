#lang racket/base

(require (prefix-in r: racket/base)
         (prefix-in reader: "lang/reader.rkt")
         "private/mutability.rkt")

(provide read)

(define (read in)
  (to-mutable (reader:r7rs-parameterize-read
               (λ () (r:read in)))))
