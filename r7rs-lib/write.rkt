#lang racket/base

(require racket/port
         racket/pretty
         "private/strip-prefix.rkt")

(provide (strip-colon-prefix-out 7:write 7:write-shared 7:display)
         (rename-out [7:write write-simple]))

(define (7:write v [out (current-output-port)])
  (do-output #t v out))
(define (7:write-shared v [out (current-output-port)])
  (parameterize ([print-graph #t])
    (do-output #t v out)))
(define (7:display v [out (current-output-port)])
  (do-output #f v out))

(define (do-output write? v out)
  (parameterize ([print-mpair-curly-braces #f]
                 [pretty-print-columns 'infinity]
                 [pretty-print-depth #f]
                 [pretty-print-pre-print-hook void]
                 [pretty-print-post-print-hook void]
                 [pretty-print-size-hook r7rs-size-hook]
                 [pretty-print-print-hook r7rs-print-hook])
    (if write?
        (pretty-write v out #:newline? #f)
        (pretty-display v out #:newline? #f))))

(define (r7rs-size-hook v display? out)
  (cond
    [(bytes? v)
     (define null-out (open-output-nowhere))
     (port-count-lines! null-out)
     (port-write-handler null-out (port-write-handler out))
     (port-display-handler null-out (port-display-handler out))
     (port-print-handler null-out (port-print-handler out))
     (r7rs-print-hook v display? null-out)
     (define-values [line col pos] (port-next-location null-out))
     (sub1 pos)]
    [else
     #f]))

(define (r7rs-print-hook v display? out)
  (cond
    [(bytes? v)
     (write-bytevector v out)]
    [else
     (error 'r7rs-print-hook "don’t know how to print ~e" v)]))

(define (write-bytevector bs out)
  (write-string "#u8(" out)
  (unless (zero? (bytes-length bs))
    (write (bytes-ref bs 0) out)
    (for ([b (in-bytes bs 1)])
      (write-char #\space out)
      (write b out)))
  (write-char #\) out)
  (void))
