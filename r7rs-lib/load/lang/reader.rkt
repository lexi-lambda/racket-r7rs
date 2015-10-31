#lang s-exp syntax/module-reader r7rs/load/lang/language

#:wrapper1 (lambda (t)
             (parameterize ([read-case-sensitive #f]
                            [read-accept-infix-dot #f]
                            [read-curly-brace-as-paren #f]
                            [read-square-bracket-as-paren #f])
               (t)))
