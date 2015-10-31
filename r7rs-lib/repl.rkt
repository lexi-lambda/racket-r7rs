#lang racket/base

(provide interaction-environment)

(define (interaction-environment)
  (current-namespace))
