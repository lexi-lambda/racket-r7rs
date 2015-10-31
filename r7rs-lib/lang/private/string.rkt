#lang racket/base

(require syntax/readerr)

(provide read-string)

(define current-source (make-parameter #f))

(define read-string
  (case-lambda
    ; read
    [(c in)
     (let loop ([chars '()])
       (let ([c (read-string-char in)])
         (if c
             (loop (cons c chars))
             (list->string (reverse chars)))))]
    ; read-syntax
    [(c in src line col pos)
     (parameterize ([current-source src])
       (let* ([datum (read-string c in)]
              [final-pos (file-position in)])
         (datum->syntax #f datum (list src line col pos (- final-pos pos)))))]))

(define (read-string-char in)
  (let ([c (read-char in)])
    (case c
      [(#\") #f]
      [(#\\) (read-escape in)]
      [else  c])))

(define (read-escape in)
  (let ([c (read-char in)])
    (case c
      [(#\a) #\u0007]
      [(#\b) #\u0008]
      [(#\t) #\tab]
      [(#\n) #\newline]
      [(#\r) #\return]
      [(#\") #\"]
      [(#\\) #\\]
      [(#\|) #\|]
      [(#\x) (read-hex-escape in)]
      [else  c])))

(define (read-hex-escape in)
  (let*-values ([(line col pos) (port-next-location in)]
                [(pos) (- pos 2)])
    (let loop ([digits '()])
      (let ([c (read-char in)])
        (case c
          [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
            #\a #\b #\c #\d #\e #\f
            #\A #\B #\C #\D #\E #\F)
           (loop (cons c digits))]
          [(#\;) (integer->char (string->number (list->string (reverse digits)) 16))]
          [else (raise-read-error "Invalid or unterminated hex escape in string" (current-source)
                                  line col pos (- (add1 (file-position in)) pos))])))))
