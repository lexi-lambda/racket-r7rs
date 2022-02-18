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
                    [read-syntax-accept-graph #t]
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

#| Note [The graph structure story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
R7RS allows graph structure literals to be embedded in program text, which means
that, for example, the following is legal:

    > (equal? '#1=(a b . #1#)
              '#2=(a b a b . #2#))
    #t

The spec states that “it is an error [...] to include circular references except
in literals,” so we do not have to provide any meaningful interpretation of, say,

    #1=(cons 1 #1#),

but unfortunately this does not allow us to determine where circular references
are permitted at `read`-time, as what constitutes a “literal” is only known
after performing macroexpansion. This leaves us in a bit of a pickle, because
Racket does not ordinarily allow circularity of any kind in syntax objects!

However, we can get close enough by simulating circularity via explicit
indirection. When `read-syntax-accept-graph` is set to #t, the reader will
permit graph structure literals and will return syntax-wrapped #<placeholder>
values in their stead. We must then take care to give such syntax objects
special treatment in three different places:

  1. First, we must provide a variant of `quote` that detects placeholders and
     expands to an expression that resolves them.

  2. Second, we must provide a binding for `#%datum` that rejects placeholders
     that appear in an expression context.

  3. Third, we must provide a variant of `syntax-rules` that unwraps
     placeholders to the extent necessary when matching patterns.

Naturally, we do all of these things.

Technically, if we were to be perfect R7RS citizens, we ought to additionally
reimplement every standard syntactic form to walk graph structure as necessary,
so for example it ought to be possible to write

    #1=(if foo bar '#1#)

and have it work. However, we do not do this, as it would be a royal pain to
implement, and it is vanishingly unlikely that anyone will ever actually do this. |#
