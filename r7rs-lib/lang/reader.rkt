#lang s-exp syntax/module-reader r7rs

#:wrapper1 r7rs-parameterize-read

(require (prefix-in reader: "private/string.rkt"))

(provide r7rs-parameterize-read)

(define (make-r7rs-readtable base)
  (make-readtable base #\" 'terminating-macro reader:read-string))

(define (r7rs-parameterize-read do-read)
  (parameterize ([read-accept-infix-dot #f]
                 [read-curly-brace-as-paren #f]
                 [read-square-bracket-as-paren #f]
                 [current-readtable (make-r7rs-readtable (current-readtable))])
    (do-read)))
