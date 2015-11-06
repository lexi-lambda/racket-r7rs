#lang racket/base

(require racket/contract)

(provide bytevector-copy string->utf8 utf8->string)

(define/contract bytevector-copy
  (case-> (bytes? . -> . bytes?)
          (bytes? exact-nonnegative-integer? . -> . bytes?)
          (bytes? exact-nonnegative-integer? exact-nonnegative-integer? . -> . bytes?))
  (case-lambda
    [(s)      (bytes-copy s)]
    [(s st)   (subbytes s st)]
    [(s st e) (subbytes s st e)]))

(define string->utf8
  (case-lambda
    [(c)     (string->bytes/utf-8 c)]
    [(c s)   (string->bytes/utf-8 c #f s)]
    [(c s e) (string->bytes/utf-8 c #f s e)]))

(define utf8->string
  (case-lambda
    [(b)     (bytes->string/utf-8 b)]
    [(b s)   (bytes->string/utf-8 b #f s)]
    [(b s e) (bytes->string/utf-8 b #f s e)]))
