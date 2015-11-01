#lang racket/base

(require compatibility/mlist
         racket/contract)

(provide vector->list vector-fill! vector-map)

(define/contract (vector->list vec [start 0] [end (vector-length vec)])
  ([vector?] [exact-nonnegative-integer? exact-nonnegative-integer?] . ->* . mlist?)
  (unless (start . >= . 0)
    (raise-range-error 'vector->list "vector" "starting " start vec 0 (vector-length vec)))
  (unless (end . >= . start)
    (raise-range-error 'vector->list "vector" "ending " end vec start (vector-length vec) 0))
  (unless ((vector-length vec) . >= . end)
    (raise-range-error 'vector->list "vector" "ending " end vec 0 (vector-length vec)))
  (for/fold ([acc null])
            ([i (in-range (sub1 end) (sub1 start) -1)])
    (mcons (vector-ref vec i) acc)))

(define/contract (vector-fill! vec v [start 0] [end (vector-length vec)])
  ([(and/c vector? (not/c immutable?)) any/c] [exact-nonnegative-integer? exact-nonnegative-integer?]
                                              . ->* . void?)
  (unless (start . >= . 0)
    (raise-range-error 'vector-fill! "vector" "starting " start vec 0 (vector-length vec)))
  (unless (end . >= . start)
    (raise-range-error 'vector-fill! "vector" "ending " end vec start (vector-length vec) 0))
  (unless ((vector-length vec) . >= . end)
    (raise-range-error 'vector-fill! "vector" "ending " end vec 0 (vector-length vec)))
  (for ([i (in-range start end)])
    (vector-set! vec i v)))

(define/contract (vector-map proc . vecs)
  ([(unconstrained-domain-> any/c)] #:rest (non-empty-listof vector?) . ->* . vector?)
  (list->vector (for/fold ([acc null])
                          ([i (in-range (sub1 (apply min (map vector-length vecs))) -1 -1)])
                  (cons (apply proc (map (λ (v) (vector-ref v i)) vecs)) acc))))
