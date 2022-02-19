#lang racket/base

(require racket/port
         syntax/readerr)

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
       (define datum (read-string c in))
       (datum->syntax
        #f
        datum
        (list src line col pos
              (and pos (let ()
                         (define-values [line* col* pos*] (port-next-location in))
                         (- pos* pos))))))]))

(define (read-string-char in)
  (let ([c (read-char in)])
    (case c
      [(#\") #f]
      [(#\\) (read-escape in)]
      [else  c])))

(define (read-escape in)
  (let ([c (peek-char in)])
    (case c
      [(#\a) (read-char in) #\u0007]
      [(#\b) (read-char in) #\u0008]
      [(#\t) (read-char in) #\tab]
      [(#\n) (read-char in) #\newline]
      [(#\r) (read-char in) #\return]
      [(#\") (read-char in) #\"]
      [(#\\) (read-char in) #\\]
      [(#\|) (read-char in) #\|]
      [(#\x) (read-char in) (read-hex-escape in)]
      [else (if (peek-only-intraline-whitespace? in)
                (read/skip-intraline-whitespace in)
                c)])))

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

(define (peek-only-intraline-whitespace? in)
  (let ([in (peeking-input-port in)])
    (let loop ()
      (case (read-char in)
        [(#\space #\tab) (loop)]
        [(#\newline)     #t]
        [else            #f]))))

(define (read/skip-intraline-whitespace in)
  (let loop ()
    (case (peek-char in)
      [(#\space #\tab) (read-char in) (loop)]
      [(#\newline)
       (read-char in)
       (let loop ()
         (case (peek-char in)
           [(#\space #\tab) (read-char in) (loop)]
           [else (read-string-char in)]))])))
