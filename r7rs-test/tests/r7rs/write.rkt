#lang r7rs

(import (scheme base)
        (scheme read)
        (scheme write)
        (rackunit))

(define (check-roundtrip val str)
  (check-equal? (let ((out (open-output-string)))
                  (write val out)
                  (get-output-string out))
                str)
  (check-equal? (read (open-input-string str))
                val))

(check-roundtrip '(1 2 3) "(1 2 3)")
(check-roundtrip '(1 . 2) "(1 . 2)")
(check-roundtrip #u8(1 2 3 4 5) "#u8(1 2 3 4 5)")
(check-roundtrip '(#u8(1 2) #u8(3 4)) "(#u8(1 2) #u8(3 4))")
