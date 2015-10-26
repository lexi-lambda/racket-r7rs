#lang racket/base
(require syntax/readerr
         (only-in "../base.rkt" bytevector))
(provide make-r7rs-readtable)

;; Error functions:
(define (bytevector-error next-char src in)
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? next-char)
         raise-read-error
         raise-read-eof-error)
     (format "bad syntax `#u~a'"
             (if (eof-object? next-char) "" next-char))
     src line col pos
     (if (eof-object? next-char) 0 1))))

(define (bytevector-eof-error next src in)
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? next)
         raise-read-error
         raise-read-eof-error)
     "Expected datum after #u8, found end of file"
     src line col pos
     (if (eof-object? next) 0 1))))

;; Reads as datum (piggybacks off of read-bytevector-syntax)
(define (read-bytevector in)
  (syntax->datum (read-bytevector-syntax #f in)))

;; Does actual bytevector reading
(define (read-bytevector-syntax src in)
  (let* ((next (peek-char in))
         (next-eof (eof-object? next)))
    (unless (equal? next #\()
      (if next-eof
          (bytevector-eof-error next src in)
          (bytevector-error (list->string (list #\8 next)) src in))))
  (with-syntax ([(vals ...) (read-syntax src in)])
    (syntax (bytevector vals ...))))

;; Dispatch function for readtable when `#u' is
;; encountered
(define readtable-bytevector
  (case-lambda
    [(ch in) ;; read
     (let ((next (peek-char in)))
       (unless (equal? next #\8)
         (bytevector-error next (object-name in) in))
       (read-char in)
       (read-bytevector in))]
    [(ch in src line col pos) ;; read-syntax
     (let ((next (peek-char in)))
       (unless (equal? next #\8)
         (bytevector-error next src in))
       (read-char in)
       (read-bytevector-syntax src in))]))

;; Adds `#u8' to the readtable
(define (make-r7rs-readtable)
  (make-readtable (current-readtable)
                  #\u 'dispatch-macro readtable-bytevector))
