#lang r7rs

(import (scheme base)
        (scheme lazy)
        (rackunit))

;; GitHub issue #9
(let ()
  (define x 0)
  (define p (delay (if (= x 5)
                       x
                       (begin
                         (set! x (+ x 1))
                         (force p)
                         (set! x (+ x 1))
                         x))))

  (check-equal? (force p) 5)
  (check-equal? (force p) 5)
  (check-equal? (force p) 5))
