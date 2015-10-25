#lang racket/base

(require racket/require
         (for-syntax racket/base
                     syntax/parse)
         (prefix-in 5: r5rs)
         (prefix-in 6: (multi-in rnrs (base-6 bytevectors-6 control-6 exceptions-6 io/ports-6)))
         (prefix-in r: (multi-in racket (base include list vector)))
         (multi-in "private" ("cond-expand.rkt" "exception.rkt" "math.rkt" "record.rkt"
                              "strip-prefix.rkt")))

(provide
 (strip-colon-prefix-out
  (for-syntax 6:_ 6:... 6:syntax-rules)
  6:* 6:+ 6:- 6:/ 6:< 6:<= 6:= 6:=> 6:> 6:>= 6:abs 6:and 6:append 6:apply 5:assoc 5:assq 5:assv
  6:begin 6:binary-port? 6:boolean=? 6:boolean? r:bytes r:bytes-append 6:bytevector-copy
  6:bytevector-copy! 6:bytevector-length 6:bytevector-u8-ref 6:bytevector-u8-set! 6:bytevector? 6:caar
  6:cadr 6:call-with-current-continuation 6:call-with-port 6:call-with-values 6:call/cc 6:car 6:case
  6:cdar 6:cddr 6:cdr 6:ceiling 6:char->integer 5:char-ready? 6:char<=? 6:char<? 6:char=? 6:char>=?
  6:char>? 6:char? 5:close-input-port 5:close-output-port 6:close-port 6:complex? 6:cond cond-expand
  6:cons 6:current-error-port 6:current-input-port 6:current-output-port 6:define define-record-type
  6:define-syntax r:define-values 6:denominator 5:display 6:do 6:dynamic-wind 6:else 6:eof-object
  6:eof-object? 6:eq? 6:equal? 6:eqv? error error-object-irritants error-object-message error-object?
  6:even? 6:exact 6:exact-integer-sqrt r:exact-integer? 6:exact? 6:expt features 6:floor
  floor-quotient floor-remainder floor/ 6:flush-output-port 6:for-each 6:gcd r:get-output-string
  6:guard 6:if include 6:inexact input-port-open? 6:input-port? 6:integer->char 6:integer? 6:lambda
  6:lcm 6:length 6:let 6:let* 6:let*-values 6:let-syntax 6:let-values 6:letrec 6:letrec*
  6:letrec-syntax 6:list 6:list->string 6:list->vector list-copy 6:list-ref list-set! 6:list-tail
  6:list? 6:make-bytevector r:make-list r:make-parameter 6:make-string 6:make-vector 6:map 6:max
  5:member 5:memq 5:memv 5:min 5:modulo 6:negative? 5:newline 6:not 6:null? 6:number->string 6:number?
  6:numerator 6:odd? r:open-input-string r:open-output-string 6:or output-port-open? 6:pair?
  r:parameterize 5:peek-char 6:port? 6:positive? 6:procedure? 6:quasiquote 6:quote 5:quotient 6:raise
  6:raise-continuable 6:rational? 6:rationalize 5:read-char
  r:read-line r:read-string 6:real? 5:remainder 6:reverse 6:round 6:set! 5:set-car! 5:set-cdr! square
  6:string 6:string->list 6:string->number 6:string->symbol string->vector 6:string-append
  6:string-copy r:string-copy! 5:string-fill! 6:string-for-each 6:string-length string-map
  6:string-ref 5:string-set! 6:string<=? 6:string<? 6:string=? 6:string>=? 6:string>? 6:string?
  6:substring 6:symbol->string 6:symbol=? 6:symbol? syntax-error 6:textual-port? 6:truncate
  truncate-quotient truncate-remainder truncate/ 6:unless 6:unquote 6:unquote-splicing 6:values
  6:vector 6:vector->list vector->string r:vector-append r:vector-copy r:vector-copy! 6:vector-fill!
  6:vector-for-each 6:vector-length 6:vector-map 6:vector-ref 6:vector-set! 6:vector 6:when
  6:with-exception-handler 5:write-char r:write-string 6:zero?)
 (rename-out [get-output-bytes get-output-bytevector]
             [exn:fail:filesystem? file-error?]
             [exn:fail:read? read-error?]
             [open-input-bytes open-input-bytevector]
             [open-output-bytes open-output-bytevector]
             [peek-byte peek-u8]
             [read-bytes read-bytevector]
             [read-bytes! read-bytevector!]
             [read-byte read-u8]
             [string->bytes/utf-8 string->utf8]
             [byte-ready? u8-ready?]
             [bytes->string/utf-8 utf8->string]
             [write-bytes write-bytevector]
             [write-byte write-u8]))

(define-syntax include
  (syntax-rules ()
    [(_ str) (r:include str)]
    [(_ str . strs)
     (begin (r:include str)
            (include . strs))]))

(define (input-port-open? port)
  (not (port-closed? port)))

(define (list-copy lst)
  (6:map values lst))

(define (list-set! lst n v)
  (let loop ([lst lst]
             [n n])
    (if (zero? n)
        (5:set-car! lst v)
        (loop (5:cdr lst) (sub1 n)))))

(define (output-port-open? port)
  (not (port-closed? port)))

(define-syntax-rule (square . _)
  (syntax-error "FIXME: not yet implemented"))

(define-syntax-rule (string->vector . _)
  (syntax-error "FIXME: not yet implemented"))

(define-syntax-rule (string-map . _)
  (syntax-error "FIXME: not yet implemented"))

(define-syntax syntax-error
  (syntax-parser
    [(_ message:str args ...)
     (apply error (syntax->datum #'message)
            (syntax->datum #'(args ...)))]))

(define-syntax-rule (vector->string . _)
  (syntax-error "FIXME: not yet implemented"))
