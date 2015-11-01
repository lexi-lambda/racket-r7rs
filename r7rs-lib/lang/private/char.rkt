#lang racket/base

(require racket/match
         syntax/readerr)

(provide (rename-out [read-char-literal read-char]))

(define current-source (make-parameter #f))

(define read-char-literal
  (case-lambda
    ; read
    [(c in)
     (let ([first-char (read-char in)])
       (cond
         [(and (equal? first-char #\x) (peek-hex? in))
          (read-char-code in)]
         [(char-alphabetic? (peek-char in))
          (read-char-name in first-char)]
         [else first-char]))]
    ; read-syntax
    [(c in src line col pos)
     (parameterize ([current-source src])
       (let* ([datum (read-char-literal c in)]
              [final-pos (file-position in)])
         (datum->syntax #f datum (list src line col pos (- final-pos pos)))))]))

(define (peek-hex? port)
  (regexp-match? #rx"[a-fA-F0-9]" (string (peek-char port))))

(define (read-char-code in)
  (let loop ([digits '()])
    (if (peek-hex? in)
        (loop (cons (read-char in) digits))
        (integer->char (string->number (list->string (reverse digits)) 16)))))

(define (read-char-name in first-char)
  (let*-values ([(line col pos) (port-next-location in)]
                [(pos) (- pos 2)])
    (let loop ([chars (list first-char)])
      (if (char-alphabetic? (peek-char in))
          (loop (cons (read-char in) chars))
          (match (list->string (reverse chars))
            ["alarm"     #\u0007]
            ["backspace" #\backspace]
            ["delete"    #\u007F]
            ["escape"    #\u001B]
            ["newline"   #\newline]
            ["null"      #\null]
            ["return"    #\return]
            ["space"     #\space]
            ["tab"       #\tab]
            [name        (raise-read-error (format "Unknown character name ~v" name)
                                           (current-source) line col pos
                                           (- (add1 (file-position in)) pos))])))))
