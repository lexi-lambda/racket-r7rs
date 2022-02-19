#lang s-exp syntax/module-reader r7rs

#:wrapper1 r7rs-parameterize-read

(require racket/require
         (prefix-in 7: "../write.rkt")
         (prefix-in reader: (multi-in "private" ("bytevector.rkt" "char.rkt" "directive.rkt"
                                                 "string.rkt" "symbol.rkt"))))

(provide r7rs-parameterize-read
         configure-runtime!)

(define (make-r7rs-readtable base)
  (make-readtable base
                  #\" 'terminating-macro reader:read-string
                  #\| 'terminating-macro reader:read-escaped-symbol
                  #\\ 'dispatch-macro reader:read-char
                  #\! 'dispatch-macro reader:read-directive
                  #\u 'dispatch-macro reader:read-bytevector
                  #f  'non-terminating-macro (reparameterize-read base)))

(define (r7rs-parameterize-read do-read)
  (call-with-default-reading-parameterization
   (λ ()
     (parameterize ([read-accept-infix-dot #f]
                    [read-curly-brace-as-paren #f]
                    [read-square-bracket-as-paren #f]
                    [current-readtable (make-r7rs-readtable (current-readtable))])
       (do-read)))))

; The #!fold-case and #!no-fold-case directives mutate the read-case-sensitive parameter, but by
; default, this doesn't affect the reader until another call to read or read-syntax is made. We call
; this function to allow this change in parameterization to take effect.
(define (reparameterize-read base)
  (case-lambda
    [(c in)                  (read/recursive            in c base)]
    [(c in src line col pos) (read-syntax/recursive src in c base)]))

(define (configure-runtime!)
  (current-read-interaction
   (λ (src in)
     (r7rs-parameterize-read
      (λ ()
        (read-syntax src in)))))

  (current-print
   (λ (val)
     (unless (void? val)
       (7:write val)
       (newline)))))
