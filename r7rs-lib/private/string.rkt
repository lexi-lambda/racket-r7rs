#lang racket/base

(require compatibility/mlist
         racket/contract
         (prefix-in r: racket/base)
         (prefix-in 6: rnrs/base-6))

(provide string->list string->vector string-copy string-fill! string-map vector->string)

(define/contract string->list
  (case-> (string? . -> . (mlistof char?))
          (string? exact-nonnegative-integer? . -> . (mlistof char?))
          (string? exact-nonnegative-integer? exact-nonnegative-integer? . -> . (mlistof char?)))
  (case-lambda
    [(s)      (6:string->list s)]
    [(s st)   (6:string->list (r:substring s st))]
    [(s st e) (6:string->list (r:substring s st e))]))

(define/contract (string->vector str [start 0] [end (string-length str)])
  ([string?] [exact-integer? exact-integer?] . ->* . vector?)
  (unless (start . >= . 0)
    (raise-range-error 'string->vector "string" "starting " start str 0 (string-length str)))
  (unless (end . >= . start)
    (raise-range-error 'string->vector "string" "ending " end str start (string-length str) 0))
  (unless ((string-length str) . >= . end)
    (raise-range-error 'string->vector "string" "ending " end str 0 (string-length str)))
  (let* ([len (- end start)]
         [vec (make-vector len)])
    (for ([vi (in-range len)]
          [si (in-range start end)])
      (vector-set! vec vi (string-ref str si)))
    vec))

(define/contract string-copy
  (case-> (string? . -> . string?)
          (string? exact-nonnegative-integer? . -> . string?)
          (string? exact-nonnegative-integer? exact-nonnegative-integer? . -> . string?))
  (case-lambda
    [(s)      (r:string-copy s)]
    [(s st)   (r:substring s st)]
    [(s st e) (r:substring s st e)]))

(define/contract (string-fill! str c [start 0] [end (string-length str)])
  ([string? char?] [exact-nonnegative-integer? exact-nonnegative-integer?] . ->* . void?)
  (unless (start . >= . 0)
    (raise-range-error 'string-fill! "string" "starting " start str 0 (string-length str)))
  (unless (end . >= . start)
    (raise-range-error 'string-fill! "string" "ending " end str start (string-length str) 0))
  (unless ((string-length str) . >= . end)
    (raise-range-error 'string-fill! "string" "ending " end str 0 (string-length str)))
  (for ([i (in-range start end)])
    (string-set! str i c)))

(define/contract (string-map proc . strs)
  ([(unconstrained-domain-> char?)] #:rest (non-empty-listof string?) . ->* . string?)
  (list->string (for/fold ([acc null])
                          ([i (in-range (sub1 (apply min (map string-length strs))) -1 -1)])
                  (cons (apply proc (map (λ (s) (string-ref s i)) strs)) acc))))

(define/contract (vector->string vec [start 0] [end (vector-length vec)])
  ([vector?] [exact-integer? exact-integer?] . ->* . string?)
  (unless (start . >= . 0)
    (raise-range-error 'vector->string "vector" "starting " start vec 0 (vector-length vec)))
  (unless (end . >= . start)
    (raise-range-error 'vector->string "vector" "ending " end vec start (vector-length vec) 0))
  (unless ((vector-length vec) . >= . end)
    (raise-range-error 'vector->string "vector" "ending " end vec 0 (vector-length vec)))
  (let* ([len (- end start)]
         [str (make-string len)])
    (for ([si (in-range len)]
          [vi (in-range start end)])
      (let ([c (vector-ref vec vi)])
        (unless (char? c)
          (raise-argument-error 'vector->string "char?" c))
        (string-set! str si c)))
    str))
