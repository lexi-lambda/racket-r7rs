#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/include
         "cond-expand.rkt"
         "export.rkt"
         "import.rkt")

(provide define-library)

; Because define-library forms use import/export/begin/cond-expand/include identifiers but are likely
; expanded in a context where those identifiers are not in scope (at least for
; begin/cond-expand/include), the library-clause syntax class adds a layer of indirection that matches
; each possible clause by datum instead of by identifier, then expands to the identifier with proper
; lexical context.

(begin-for-syntax
  (define-syntax-class library-clause
    #:attributes [with-ctx]
    [pattern ({~datum import} . body) #:attr with-ctx #'(import . body)]
    [pattern ({~datum export} . body) #:attr with-ctx #'(export . body)]
    [pattern ({~datum begin} . body) #:attr with-ctx #'(begin . body)]
    [pattern ({~datum cond-expand} . body) #:attr with-ctx #'(cond-expand . body)]
    [pattern ({~datum include} . body)
             #:attr with-ctx #`(include-at/relative-to #,this-syntax #,this-syntax . body)]))

(define-syntax (define-library stx)
  (unless (eq? (syntax-local-context) 'module)
    (raise-syntax-error 'define-library "not in module context" stx))
  (syntax-parse stx
    [(_ _ clause:library-clause ...)
     #'(begin clause.with-ctx ...)]))
