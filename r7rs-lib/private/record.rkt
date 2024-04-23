#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse))

(provide define-record-type)

(begin-for-syntax
  (define current-record-definition (make-parameter #f))
  (define current-record-name (make-parameter #f))
  (define current-record-fields (make-parameter #f))
  
  (define-syntax-class record-constructor
    #:attributes [name (field 1) fields]
    (pattern [name:id field:id ...]
             #:attr fields (syntax->list #'(field ...))))

  (define-syntax-class record-field
    #:attributes [name accessor modifier]
    (pattern [name:id accessor:id] #:attr modifier #f)
    (pattern [name:id accessor:id modifier:id]))

  ; A reified version of the record-field syntax class.
  (struct field (name accessor modifier))

  ; Raises a syntax error if the given field id isn't in (current-record-fields), otherwise does
  ; nothing.
  ; identifier? -> void?
  (define (assert-constructor-field-exists! field-id)
    (unless (member field-id (map field-name (current-record-fields)) free-identifier=?)
      (raise-syntax-error 'define-record-type
                          (format "‘~s’ is not a field of record type ‘~s’"
                                  (syntax->datum field-id)
                                  (syntax->datum (current-record-name)))
                          (current-record-definition)
                          field-id)))

  ; Gets the index for a record field (in current-record-fields) given a particular field name.
  ; identifier? -> nonnegative-integer?
  (define (index-for-field-name field-id)
    (let* ([field-names (map field-name (current-record-fields))]
           [indexed-fields (map cons field-names (range (length field-names)))])
      (cdr (assoc field-id indexed-fields free-identifier=?)))))

(define-syntax (define-record-type stx)
  (syntax-parse stx
    [(_ name:id
        constructor:record-constructor
        pred?:id
        field-spec:record-field ...)
     (parameterize ([current-record-definition stx]
                    [current-record-name #'name]
                    [current-record-fields (let ([names (attribute field-spec.name)]
                                                 [accessors (attribute field-spec.accessor)]
                                                 [modifiers (attribute field-spec.modifier)])
                                             (map field names accessors modifiers))])

       ; ensure all the constructor fields actually exist
       (for ([field (in-list (attribute constructor.fields))])
         (assert-constructor-field-exists! field))

       ; create some convenient template references to the accessor and modifier names
       (define/with-syntax (field-accessors ...)
         (map field-accessor (current-record-fields)))
       (define/with-syntax (field-modifiers ...)
         (filter identity (map field-modifier (current-record-fields))))
       (define/with-syntax (field-index ...)
         (map index-for-field-name (map field-name (current-record-fields))))
       (define/with-syntax (field-modifier-index ...)
         (map index-for-field-name (map field-name (filter field-modifier (current-record-fields)))))

       ; generate the definitions
       #`(begin
           (define-values (struct:record make-record pred? field-accessors ... field-modifiers ...)
             (let-values ([(struct:record make-record record-pred? record-ref record-set!)
                           (make-struct-type 'name #f #,(length (current-record-fields))
                                             0 #f '() #f #f '() #f 'constructor.name)])
               (values struct:record make-record record-pred?
                       (make-struct-field-accessor record-ref field-index 'field-accessors)
                       ...
                       (make-struct-field-mutator record-set! field-modifier-index 'field-modifiers ...)
                       ...)))
           ; wrap the struct constructor with a custom proc that handles missing fields
           (define (constructor.name constructor.field ...)
             (make-record #,@(for/list ([field (in-list (current-record-fields))])
                               (if (member (field-name field)
                                           (attribute constructor.fields)
                                           free-identifier=?)
                                   (field-name field)
                                   #f))))))]))

(module+ test
  (require racket/block
           racket/function
           rackunit
           "cond-expand.rkt")

  (block (define-record-type foo [make-foo a b] foo?
           [a get-a] [b get-b set-b!])
         
         (check-equal? (get-a (make-foo 1 2)) 1)
         (check-equal? (get-b (make-foo 1 2)) 2)

         (let ([foo (make-foo 1 2)])
           (set-b! foo 3)
           (check-equal? (get-b foo) 3)))

  ; the syntax/macro-testing module is unavailable before 6.3, so we just shouldn't run that test
  (cond-expand
   [(library (syntax macro-testing))
    (require syntax/macro-testing)
    (test-case
     "constructor field validation"
     (check-exn
      #rx"^define-record-type: ‘b’ is not a field of record type ‘foo’$"
      (thunk (convert-syntax-error (block (define-record-type foo [make-foo a b] foo?
                                            [a get-a]))))))]))
