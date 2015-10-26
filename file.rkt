#lang racket/base

(require racket/require
         (prefix-in r: racket/base)
         (prefix-in 6: (multi-in rnrs (files-6 io/simple-6)))
         "private/strip-prefix.rkt")

(provide
 (strip-colon-prefix-out
  6:call-with-input-file 6:call-with-output-file 6:delete-file 6:file-exists? open-binary-input-file
  open-binary-output-file 6:open-input-file 6:open-output-file 6:with-input-from-file
  6:with-output-to-file))

(define (open-binary-input-file path)
  (r:open-input-file path #:mode 'binary))

(define (open-binary-output-file path)
  (r:open-output-file path #:mode 'binary))
