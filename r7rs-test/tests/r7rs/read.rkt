#lang r7rs

(import (scheme base)
        (scheme read)
        (rackunit))

(check-equal? (parameterize ((current-input-port (open-input-string "(1 2 3)")))
                (read))
              '(1 2 3))
