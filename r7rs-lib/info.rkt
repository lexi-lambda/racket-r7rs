#lang info

(define collection "r7rs")

(define version "1.1")

(define deps
  '(["base" #:version "8.4.0.8"]
    "compatibility-lib"
    "r5rs-lib"
    "r6rs-lib"))
(define build-deps
  '("rackunit-lib"))
