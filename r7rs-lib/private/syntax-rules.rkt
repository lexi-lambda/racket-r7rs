#lang racket/base

#| This module implements R7RS’s flavor of `syntax-rules`. It is mostly
identical to the version used in `racket/base`, R5RS, and R6RS, but
unfortunately we cannot reuse that version because R7RS supports graph structure
literals directly in program text (see Note [The graph structure story] in
r7rs/lang/reader for an overview).

The implementation here is loosely based on the one used in `racket/base`, but
it is somewhat simpler and less efficient. The overall strategy is as follows:

  1. First, we use `syntax/parse` to parse the grammar of `syntax-rules` itself.

  2. We translate each `syntax-rules` pattern into a corresponding *matcher*,
     which is a combination of two things:

       a. A *pattern expression*, which evaluates to a runtime (relative to the
          `syntax-rules` form itself) AST that represents the desugared pattern.

       b. A *binder*, which is a `syntax-case` pattern used to actually bind the
          results of evaluating the pattern expression to pattern variables.

  3. Finally, we expand to uses of `interpret-pattern`, which each apply a
     pattern expression to the input syntax. We bind the results of the first
     successful match using the binder pattern and `with-syntax`.

Note that this strategy has a flaw: since the parsing of the `syntax-rules` form
is itself implemented using `syntax/parse`, it will not handle circular syntax!
This means that, technically, things like this will not work correctly:

    (define-syntax m
      (syntax-rules ()
        ((_ #0=42 #0#) ....)))

However, doing such things is asking for trouble, and really, the specification
is pretty vague as to whether such constructions are even allowed, so we
consider this an acceptable shortcoming. |#

(require (for-syntax racket/base
                     racket/match
                     syntax/id-set
                     syntax/intdef)
         racket/match
         racket/stxparam
         syntax/parse/define)

(provide syntax-rules _ ...)

;; -----------------------------------------------------------------------------

;; Recursively unwraps its argument until it is neither a syntax object nor a placeholder.
(define (syntax-e* v)
  (cond
    [(syntax? v)
     (syntax-e* (syntax-e v))]
    [(placeholder? v)
     (syntax-e* (placeholder-get v))]
    [else
     v]))

;; Splits a (possibly improper) syntax list into two pieces such that the second
;; piece contains the final `tail-len` elements (not including the final `cdr`).
;;
;; If successful, the first result is always a bona fide `list?`, but the second
;; result may be a syntax object or placeholder that corresponds to the rest of
;; the list. If unsuccessful (because the list does not contain enough
;; elements), returns `(values #f #f)`.
(define (split-stx-list* e tail-len)
  (define-values [head tail tail-size]
    (let loop ([e e])
      (match (syntax-e* e)
        [(cons a b)
         (define-values [head tail tail-size] (loop b))
         (if (= tail-size tail-len)
             (values (cons a head) tail tail-size)
             (values '() e (add1 tail-size)))]
        [v
         (values '() e 0)])))
  (if (= tail-size tail-len)
      (values head tail)
      (values #f #f)))

;; -----------------------------------------------------------------------------
;; pattern AST

(struct pat:underscore () #:prefab)
(struct pat:bind () #:prefab)
(struct pat:literal (id) #:prefab)
(struct pat:datum (value) #:prefab)
(struct pat:null () #:prefab)
(struct pat:pair (car-pat car-binds? cdr-pat cdr-binds?) #:prefab)
(struct pat:ellipsis (head-pat head-binds? tail-len tail-pat tail-binds?) #:prefab)
(struct pat:vector (list-pat) #:prefab)

;; Applies a pattern AST to a syntax object. Returns the values for any bound
;; pattern variables upon success, or returns #f upon failure.
(define (interpret-pattern stx pat)
  (let loop ([ctx (if (syntax? stx) stx #f)]
             [e stx]
             [pat pat])
    (match pat
      [(pat:underscore)
       #t]
      [(pat:bind)
       (datum->syntax ctx e ctx)]
      [(pat:literal id)
       (and (identifier? e)
            (free-identifier=? e id))]
      [(pat:datum val)
       (equal? (syntax-e* e) val)]
      [(pat:null)
       (null? (syntax-e* e))]

      [(pat:pair car-pat car-binds? cdr-pat cdr-binds?)
       (let ([ctx (if (syntax? e) e ctx)]
             [e (syntax-e* e)])
         (and (pair? e)
              (let ([car-result (loop ctx (car e) car-pat)])
                (and car-result
                     (let ([cdr-result (loop ctx (cdr e) cdr-pat)])
                       (and cdr-result
                            (if car-binds?
                                (if cdr-binds?
                                    (cons car-result cdr-result)
                                    car-result)
                                cdr-result)))))))]

      [(pat:ellipsis head-pat head-binds?
                     tail-len tail-pat tail-binds?)
       (let ([ctx (if (syntax? e) e ctx)])
         (define-values [heads tail] (split-stx-list* e tail-len))
         (and heads
              (let ([head-results
                     (let head-loop ([heads heads]
                                     [head-results (if head-binds? '() #t)])
                       (cond
                         [(null? heads)
                          (if head-binds?
                              (reverse head-results)
                              #t)]
                         [else
                          (define head-result (loop ctx (car heads) head-pat))
                          (and head-result
                               (head-loop (cdr heads)
                                          (if head-binds?
                                              (cons head-result head-results)
                                              #t)))]))])
                (and head-results
                     (let ([tail-result (loop ctx tail tail-pat)])
                       (and tail-result
                            (if head-binds?
                                (if tail-binds?
                                    (cons head-results tail-result)
                                    head-results)
                                tail-result)))))))]

      [(pat:vector list-pat)
       (let ([ctx (if (syntax? e) e ctx)]
             [e (syntax-e* e)])
         (and (vector? e)
              (loop ctx (vector->list e) list-pat)))])))

(begin-for-syntax
  (struct matcher
    (binder ; (or/c syntax? #f): a `syntax-case` pattern that can be used to
            ;   bind the result of interpreting `pat-e`; #f iff `pat-e` does not
            ;   bind any variables
     pat-e) ; syntax?: an expression that evaluates to a pattern AST that can be
            ;   interpreted by `interpret-pattern`
    #:transparent)

  (define (binder-cons a b)
    (if a
        (if b
            #`(#,a . #,b)
            a)
        b))

  (define underscore-matcher (matcher #f #'(pat:underscore)))
  (define null-matcher (matcher #f #'(pat:null)))

  (define (list*-matcher head-ms rest-m)
    (for/foldr ([rest-binder (matcher-binder rest-m)]
                [rest-pat-e (matcher-pat-e rest-m)]
                #:result (matcher rest-binder rest-pat-e))
               ([head-m (in-list head-ms)])
      (match-define (matcher head-binder head-pat-e) head-m)
      (values (binder-cons head-binder rest-binder)
              #`(pat:pair #,head-pat-e #,(and head-binder #t)
                          #,rest-pat-e #,(and rest-binder #t)))))

  (define (ellipsis-matcher pre-ms head-ms post-ms rest-m)
    (match-define (matcher head-binder head-pat-e) head-ms)
    (match-define (matcher tail-binder tail-pat-e) (list*-matcher post-ms rest-m))
    (list*-matcher
     pre-ms
     (matcher (binder-cons (and head-binder
                                #`(#,head-binder (... ...)))
                           tail-binder)
              #`(pat:ellipsis #,head-pat-e #,(and head-binder #t)
                              #,(length post-ms)
                              #,tail-pat-e #,(and tail-binder #t)))))

  (define (vector-matcher list-m)
    (matcher (matcher-binder list-m)
             #`(pat:vector #,(matcher-pat-e list-m))))

  (define current-literal-ids (gensym 'literal-ids))

  (define-syntax-class pat
    #:description "pattern"
    #:attributes [m]
    #:commit
    (pattern :non-pair-pat)
    (pattern (pre:pat ... head:pat _:ellipsis post:pat ... . rest:non-pair-pat)
      #:attr m (ellipsis-matcher (attribute pre.m)
                                 (attribute head.m)
                                 (attribute post.m)
                                 (attribute rest.m)))
    (pattern (head:pat ...+ . rest:non-pair-pat)
      #:attr m (list*-matcher (attribute head.m) (attribute rest.m))))

  (define-syntax-class non-pair-pat
    #:description #f
    #:attributes [m]
    #:commit
    (pattern x:id
      #:when (free-id-set-member? (syntax-parse-state-ref current-literal-ids) #'x)
      #:do [(syntax-parse-state-cons! 'literals #'x)]
      #:attr m (matcher #f #'(pat:literal (quote-syntax x))))
    (pattern {~literal _}
      #:attr m underscore-matcher)
    (pattern {~and x:id {~not _:ellipsis}}
      #:attr m (matcher (if (free-identifier=? #'x (quote-syntax ...))
                            #'((... ...) x) ; see Note [Custom ellipsis identifiers]
                            #'x)
                        #'(pat:bind)))
    (pattern v:pat-datum
      #:attr m (matcher #f #'(pat:datum 'v)))
    (pattern ()
      #:attr m null-matcher)

    (pattern #(pre:pat ... head:pat _:ellipsis post:pat ...)
      #:attr m (vector-matcher (ellipsis-matcher (attribute pre.m)
                                                 (attribute head.m)
                                                 (attribute post.m)
                                                 null-matcher)))
    (pattern #(head:pat ...)
      #:attr m (vector-matcher (list*-matcher (attribute head.m) null-matcher))))

  (define-syntax-class (template #:escaped? [escaped? #f])
    #:attributes [ds]
    #:commit
    (pattern _:ellipsis
      #:when (not escaped?)
      #:attr ds (quote-syntax ...))
    (pattern ooo:id #:when (free-identifier=? #'ooo (quote-syntax ...))
      #:when (not escaped?)
      #:attr ds (syntax/loc #'ooo
                  ((... ...) ooo)))
    (pattern (_:ellipsis {~var t (template #:escaped? #t)})
      #:attr ds (syntax/loc this-syntax
                  ((... ...) t.ds)))
    (pattern {~or* t:pat-datum t:id}
      #:attr ds #'t)
    (pattern ({~var t (template #:escaped? escaped?)} ...)
      #:attr ds (datum->syntax this-syntax (attribute t.ds) this-syntax this-syntax))
    (pattern ({~var a (template #:escaped? escaped?)} ...+
              . {~var b (template #:escaped? escaped?)})
      #:attr ds (datum->syntax this-syntax
                               (append (attribute a.ds) (attribute b.ds))
                               this-syntax
                               this-syntax))
    (pattern #({~var t (template #:escaped? escaped?)} ...)
      #:attr ds (datum->syntax this-syntax
                               (apply vector-immutable (attribute t.ds))
                               this-syntax
                               this-syntax)))

  (define-syntax-class pat-datum
    #:description #f
    #:attributes []
    #:commit
    (pattern {~or* _:string _:char _:boolean _:number}))

  ; see Note [Custom ellipsis identifiers]
  (define-syntax-class ellipsis
    #:opaque
    #:attributes []
    #:commit
    (pattern x:id
      #:when (free-identifier=? #'x (syntax-parameter-value #'current-ellipsis))
      #:do [(syntax-parse-state-cons! 'literals #'x)]))

  (define-syntax-class rule
    #:attributes [m rhs]
    #:commit
    (pattern [(_ . pat:pat) rhs-t:template]
      #:attr m (list*-matcher (list underscore-matcher) (attribute pat.m))
      #:attr rhs (syntax/loc #'rhs-t
                   (syntax rhs-t.ds)))))

; see Note [Custom ellipsis identifiers]
(define-syntax-parameter current-ellipsis (quote-syntax ...))

(define-syntax-parser syntax-rules
  #:track-literals
  ; see Note [Custom ellipsis identifiers]
  [(_ ellipsis:id (literal:id ...) rule ...)
   #`(let-syntax ([ellipsis #f])
       (syntax-parameterize ([current-ellipsis (quote-syntax ellipsis)])
         #,(syntax/loc this-syntax
             (syntax-rules (literal ...) rule ...))))]

  [(_ (literal:id ...)
      {~do (syntax-parse-state-set! current-literal-ids
                                    (immutable-free-id-set (attribute literal)))}
      rule:rule ...)

   (quasisyntax/loc this-syntax
     (λ (stx)
       #,(for/foldr ([fail-e #'(raise-syntax-error #f "bad syntax" stx)])
                    ([m (in-list (attribute rule.m))]
                     [rhs (in-list (attribute rule.rhs))])
           #`(let ([result (interpret-pattern stx #,(matcher-pat-e m))])
               (if result
                   #,(if (matcher-binder m)
                         #`(with-syntax ([#,(matcher-binder m) result])
                             #,rhs)
                         rhs)
                   #,fail-e)))))])

#| Note [Custom ellipsis identifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
R7RS’s `syntax-rules` has the somewhat unusual feature that the ellipsis
identifier can be customized by writing

    (syntax-rules ooo () ....)

which uses `ooo` instead of `...`. To implement this, we do two things:

  1. First, when an `ooo` identifier is provided, we expand into a `let-syntax`
     form that creates a binding for it, then sets the `current-ellipsis`
     syntax parameter appropriately and trampolines into a use of `syntax-rules`
     without an `ooo` identifier.

  2. Then, when parsing patterns and templates, we look for the identifier
     stored in the `current-ellipsis` syntax parameter instead of
     unconditionally looking for `...`.

The main tricky part of this process is appropriately expanding to uses of
`syntax` in the template, because we need to ensure the real `...` identifier is
not “active” if it has been customized to something else. This is still fairly
mechanical, but it does require some subtle quoting to get right.


One subtlety is that if a custom `ooo` identifier is provided, `...` may in fact
be used as a pattern variable! For example, we can write

    (syntax-rules ooo ()
      ((_ ...) ...))

which is the identity macro. We have to take care to get this right, because we
have to escape the generated `with-syntax` pattern to ensure the `...` is not
treated as a repetition. |#
