#lang s-exp syntax/module-reader r7rs

#:wrapper1 (lambda (t)
             (parameterize ([current-readtable (make-r7rs-readtable)]
                            [read-accept-infix-dot #f]
                            [read-curly-brace-as-paren #f]
                            [read-square-bracket-as-paren #f])
               (t)))

(require "r7rs-reader.rkt")
(current-read-interaction r7rs-read-interaction)
