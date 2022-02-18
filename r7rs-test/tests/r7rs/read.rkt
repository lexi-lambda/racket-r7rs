#lang r7rs

(import (scheme base)
        (scheme read)
        (scheme private rackunit))

(check-equal? (parameterize ((current-input-port (open-input-string "(1 2 3)")))
                (read))
              '(1 2 3))

;; -----------------------------------------------------------------------------
;; graph structure in program text (see #17)

(check-equal? (car '#1=(a b . #1#)) 'a)
(check-equal? '#1=(a b . #1#) '#1#)
(check-equal? '#1=(a b . #1#) '#2=(a b a b . #2#))

(check-exn
 #rx"^#%datum: unquoted datum label$"
 (lambda ()
   (convert-syntax-error
    (car #1=(a b . #1#)))))

(let ()
  (define-syntax quoted-car
    (syntax-rules (quote)
      ((_ (quote (a . b)))
       (quote a))))

  (check-equal? (quoted-car '#0=(1 . #0#)) 1))

(let ()
  (define-syntax let-and
    (syntax-rules ()
      ((_ x e1 e2)
       (let ((x e1))
         (and x e2)))))

  (check-equal? (let-and x '#0=(1 . #0#) (car x)) 1))
