#lang s-exp syntax/module-reader r7rs

#:wrapper1 r7rs-parameterize-read

(require racket/require
         (prefix-in reader: (multi-in "private" ("char.rkt" "string.rkt"))))

(provide r7rs-parameterize-read)

(define (make-r7rs-readtable base)
  (make-readtable base
                  #\" 'terminating-macro reader:read-string
                  #\\ 'dispatch-macro reader:read-char))

(define (r7rs-parameterize-read do-read)
  (parameterize ([read-accept-infix-dot #f]
                 [read-curly-brace-as-paren #f]
                 [read-square-bracket-as-paren #f]
                 [current-readtable (make-r7rs-readtable (current-readtable))])
    (do-read)))
