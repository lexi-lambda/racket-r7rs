#lang racket/base

(require (prefix-in 7: r7rs/base)
         rackunit)

(check-not-false (7:memq (system-type 'os) (7:features)))

(check-equal? (7:cond-expand
                [windows 'windows]
                [macosx  'macosx]
                [unix    'unix])
              (system-type 'os))
