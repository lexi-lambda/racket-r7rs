#lang racket/base

(require racket/require
         (prefix-in r: racket/base)
         "private/strip-prefix.rkt")

(provide
 (strip-colon-prefix-out
  r:call-with-input-file r:call-with-output-file r:delete-file r:file-exists? open-binary-input-file
  open-binary-output-file r:open-input-file r:open-output-file r:with-input-from-file
  r:with-output-to-file))

(define (open-binary-input-file path)
  (r:open-input-file path #:mode 'binary))

(define (open-binary-output-file path)
  (r:open-output-file path #:mode 'binary))
