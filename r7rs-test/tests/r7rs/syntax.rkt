#lang r7rs

(import (scheme base)
        (rackunit))

(let ()
  (define-syntax or
    (syntax-rules ooo ()
      ((or) #t)
      ((or a) a)
      ((or a b)
       (let ((tmp a))
         (if tmp tmp b)))
      ((or a b c ooo)
       (or (or a b) c ooo))))

  (check-equal? (or #f #f 3 #f 4 #f) 3))

(let ()
  (define-syntax define-m
    (syntax-rules ooo ()
      ((_ x ooo)
       (begin
         (define-syntax x
           (syntax-rules ()
             ((_ e ...)
              (list 'e ...))))
         ooo))))

  (define-m m)
  (check-equal? (m (a b) (c d)) '((a b) (c d))))

(let ()
  (define-syntax define-m
    (syntax-rules ooo ()
      ((_ ... ooo)
       (begin
         (define-syntax ...
           (syntax-rules OOO ()
             ((_ e OOO)
              (list 'e OOO))))
         ooo))))

  (define-m m)
  (check-equal? (m (a b) (c d)) '((a b) (c d))))
