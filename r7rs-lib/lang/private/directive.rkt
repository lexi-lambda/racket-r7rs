#lang racket/base

(require racket/contract
         racket/match
         syntax/readerr)

(provide read-directive)

(define (read-directive c in src line col pos)
  (let ([directive (let loop ([chars '()])
                     (if (peek-delimiter? in)
                         (list->string (reverse chars))
                         (loop (cons (read-char in) chars))))])
    (match directive
      ["fold-case" (read-case-sensitive #f)]
      ["no-fold-case" (read-case-sensitive #t)]
      [_ (raise-read-error (format "Unknown reader directive ‘#!~a’" directive)
                           src line col pos (- (add1 (file-position in)) pos))]))
  (make-special-comment #f))

(define delimiter?
  (or/c eof-object? char-whitespace?
        #\| #\( #\) #\" #\;))

(define (peek-delimiter? port)
  (delimiter? (peek-char port)))
