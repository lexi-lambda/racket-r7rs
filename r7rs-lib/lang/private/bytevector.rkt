#lang racket/base

(require syntax/readerr)

(provide read-bytevector)

(define current-source (make-parameter #f))

(define read-bytevector
  (case-lambda
    ; read
    [(c in)
     (verify-bytevector-prefix! in)
     (let loop ([chars '()])
       (let ([bs (read-byte-literals in)])
         (list->bytes bs)))]
    ; read-syntax
    [(c in src line col pos)
     (parameterize ([current-source src])
       (let* ([datum (read-bytevector c in)]
              [final-pos (file-position in)])
         (datum->syntax #f datum (list src line col pos (- final-pos pos)))))]))

(define (verify-bytevector-prefix! in)
  (let*-values ([(line col pos) (port-next-location in)]
                [(c) (read-char in)])
    (unless (eqv? c #\8)
      (raise-read-error (format "bad syntax `#u~a'" c) (current-source)
                        line col pos (- (add1 (file-position in)) pos)))))

(define (read-byte-literals in)
  (let* ([v (read-syntax (current-source) in)]
         [d (syntax->datum v)])
    (unless (list? d)
      (raise-read-error (format "expected a list of bytevector elements, given ~v" d)
                        (syntax-source v) (syntax-line v) (syntax-column v)
                        (syntax-position v) (syntax-span v)))
    (for* ([e (in-list (syntax->list v))]
           [d (in-value (syntax->datum e))])
      (unless (and (exact-integer? d) (<= 0 d 255))
        (raise-read-error "bytevector element must be an exact integer between 0 and 255"
                          (syntax-source e) (syntax-line e) (syntax-column e)
                          (syntax-position e) (syntax-span e))))
    d))
