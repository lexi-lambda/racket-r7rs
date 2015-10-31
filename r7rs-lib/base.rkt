#lang racket/base

(require racket/contract
         racket/require
         (for-syntax (for-syntax racket/base
                                 syntax/parse)
                     (except-in racket/base syntax-rules)
                     racket/syntax
                     syntax/parse)
         (prefix-in 5: r5rs)
         (prefix-in 6: (multi-in rnrs (base-6 bytevectors-6 control-6 exceptions-6 io/ports-6)))
         (prefix-in r: (multi-in racket (base include list math vector)))
         (multi-in "private" ("case.rkt" "cond-expand.rkt" "define-values.rkt" "exception.rkt"
                              "math.rkt" "record.rkt" "strip-prefix.rkt")))

(provide
 (strip-colon-prefix-out
  (for-syntax 6:_ 6:... syntax-rules)
  6:* 6:+ 6:- 6:/ 6:< 6:<= 6:= 6:=> 6:> 6:>= 6:abs 6:and 6:append 6:apply 5:assoc 5:assq 5:assv
  6:begin 6:binary-port? 6:boolean=? 6:boolean? r:bytes r:bytes-append 6:bytevector-copy
  6:bytevector-copy! 6:bytevector-length 6:bytevector-u8-ref 6:bytevector-u8-set! 6:bytevector? 6:caar
  6:cadr 6:call-with-current-continuation 6:call-with-port 6:call-with-values 6:call/cc 6:car case
  6:cdar 6:cddr 6:cdr 6:ceiling 6:char->integer 5:char-ready? 6:char<=? 6:char<? 6:char=? 6:char>=?
  6:char>? 6:char? 5:close-input-port 5:close-output-port 6:close-port 6:complex? 6:cond cond-expand
  6:cons 6:current-error-port 6:current-input-port 6:current-output-port 6:define define-record-type
  6:define-syntax define-values 6:denominator 6:do 6:dynamic-wind 6:else 6:eof-object 6:eof-object?
  6:eq? 6:equal? 6:eqv? error error-object-irritants error-object-message error-object? 6:even?
  6:exact 6:exact-integer-sqrt r:exact-integer? 6:exact? 6:expt features 6:floor floor-quotient
  floor-remainder floor/ 6:for-each 6:gcd r:get-output-string 6:guard 6:if include 6:inexact
  6:inexact? input-port-open? 6:input-port? 6:integer->char 6:integer? 6:lambda 6:lcm 6:length 6:let
  6:let* 6:let*-values 6:let-syntax 6:let-values 6:letrec 6:letrec* 6:letrec-syntax 6:list
  6:list->string 6:list->vector list-copy 6:list-ref list-set! 6:list-tail 6:list? 6:make-bytevector
  r:make-list r:make-parameter 6:make-string 6:make-vector 6:map 6:max 5:member 5:memq 5:memv 5:min
  5:modulo 6:negative? 5:newline 6:not 6:null? 6:number->string 6:number? 6:numerator 6:odd?
  r:open-input-string r:open-output-string 6:or 6:output-port? output-port-open? 6:pair?
  r:parameterize 5:peek-char 6:port? 6:positive? 6:procedure? 6:quasiquote 6:quote 5:quotient 6:raise
  6:raise-continuable 6:rational? 6:rationalize 5:read-char r:read-line r:read-string 6:real?
  5:remainder 6:reverse 6:round 6:set! 5:set-car! 5:set-cdr! 6:string 6:string->list 6:string->number
  6:string->symbol string->vector 6:string-append 6:string-copy r:string-copy! 5:string-fill!
  6:string-for-each 6:string-length string-map 6:string-ref 5:string-set! 6:string<=? 6:string<?
  6:string=? 6:string>=? 6:string>? 6:string? 6:substring 6:symbol->string 6:symbol=? 6:symbol?
  syntax-error 6:textual-port? 6:truncate truncate-quotient truncate-remainder truncate/ 6:unless
  6:unquote 6:unquote-splicing 6:values 6:vector 6:vector->list vector->string r:vector-append
  r:vector-copy r:vector-copy! 6:vector-fill! 6:vector-for-each 6:vector-length 6:vector-map
  6:vector-ref 6:vector-set! 6:vector? 6:when 6:with-exception-handler 5:write-char r:write-string
  6:zero?)
 (rename-out [r:bytes bytevector]
             [r:get-output-bytes get-output-bytevector]
             [r:exn:fail:filesystem? file-error?]
             [r:flush-output flush-output-port]
             [r:exn:fail:read? read-error?]
             [r:open-input-bytes open-input-bytevector]
             [r:open-output-bytes open-output-bytevector]
             [r:peek-byte peek-u8]
             [r:read-bytes read-bytevector]
             [r:read-bytes! read-bytevector!]
             [r:read-byte read-u8]
             [r:sqr square]
             [r:string->bytes/utf-8 string->utf8]
             [r:byte-ready? u8-ready?]
             [r:bytes->string/utf-8 utf8->string]
             [r:write-bytes write-bytevector]
             [r:write-byte write-u8]))

(define-syntax (include stx)
  (syntax-parse stx
    [(_ str ...+)
     ; make sure each include form has the right lexical context
     (define/with-syntax (inc ...)
       (for/list ([path (in-list (attribute str))])
         (datum->syntax stx `(,#'r:include ,path) path)))
     #'(begin inc ...)]))

(define/contract (input-port-open? port)
  (input-port? . -> . boolean?)
  (not (port-closed? port)))

(define/contract (list-copy lst)
  (6:list? . -> . 6:list?)
  (6:map values lst))

(define/contract (list-set! lst n v)
  (6:list? exact-nonnegative-integer? any/c . -> . void?)
  (let loop ([lst lst]
             [n n])
    (if (zero? n)
        (5:set-car! lst v)
        (loop (5:cdr lst) (sub1 n)))))

(define/contract (output-port-open? port)
  (output-port? . -> . boolean?)
  (not (port-closed? port)))

(define/contract (string->vector str [start 0] [end (string-length str)])
  ([string?] [exact-integer? exact-integer?] . ->* . vector?)
  (unless (start . >= . 0)
    (raise-range-error 'string->vector "string" "starting " start str 0 (string-length str)))
  (unless (end . >= . start)
    (raise-range-error 'string->vector "string" "ending " end str start (string-length str) 0))
  (unless ((string-length str) . >= . end)
    (raise-range-error 'string->vector "string" "ending " end str 0 (string-length str)))
  (let* ([len (- end start)]
         [vec (make-vector len)])
    (for ([vi (in-range len)]
          [si (in-range start end)])
      (vector-set! vec vi (string-ref str si)))
    vec))

(define/contract (string-map proc str0 . strs)
  ([(unconstrained-domain-> char?) string?] #:rest (listof string?) . ->* . string?)
  (list->string (apply map proc (map string->list (cons str0 strs)))))

(define-syntax syntax-error
  (syntax-parser
    [(_ message:str args ...)
     (apply error (syntax->datum #'message)
            (syntax->datum #'(args ...)))]))

(begin-for-syntax
  (define-syntax syntax-rules
    (syntax-parser
      [(_ dots:id (literal:id ...) clause ...)
       #'(let-syntax ([dots (make-rename-transformer #'((... ...) (... ...)))])
           (5:syntax-rules (literal ...)
                           clause ...))]
      [(_ (literal:id ...) clause ...)
       #'(5:syntax-rules (literal ...)
                         clause ...)])))

(define/contract (vector->string vec [start 0] [end (vector-length vec)])
  ([vector?] [exact-integer? exact-integer?] . ->* . string?)
  (unless (start . >= . 0)
    (raise-range-error 'vector->string "vector" "starting " start vec 0 (vector-length vec)))
  (unless (end . >= . start)
    (raise-range-error 'vector->string "vector" "ending " end vec start (vector-length vec) 0))
  (unless ((vector-length vec) . >= . end)
    (raise-range-error 'vector->string "vector" "ending " end vec 0 (vector-length vec)))
  (let* ([len (- end start)]
         [str (make-string len)])
    (for ([si (in-range len)]
          [vi (in-range start end)])
      (let ([c (vector-ref vec vi)])
        (unless (char? c)
          (raise-argument-error 'vector->string "char?" c))
        (string-set! str si c)))
    str))
