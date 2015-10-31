#lang racket/base

(provide current-jiffy jiffies-per-second current-second)

(define (current-jiffy)
  (current-milliseconds))

(define (jiffies-per-second)
  1000)

(define (current-second)
  (* 0.001 (current-inexact-milliseconds)))
