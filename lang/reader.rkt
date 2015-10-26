#lang s-exp syntax/module-reader r7rs

#:wrapper1 (lambda (t)
             (parameterize ([current-readtable (r7rs:make-r7rs-readtable)]
                            [read-accept-infix-dot #f]
                            [read-curly-brace-as-paren #f]
                            [read-square-bracket-as-paren #f])
               (t)))

(require (prefix-in r7rs: "r7rs-reader.rkt"))