#lang racket/base

(require (prefix-in r: racket/base)
         (prefix-in reader: "lang/reader.rkt"))

(provide read)

(define (read in)
  (reader:r7rs-parameterize-read
   (λ () (r:read in))))
