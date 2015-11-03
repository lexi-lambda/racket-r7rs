#lang racket/base

(provide to-immutable to-mutable)

(define (to-immutable p)
  (cond
    [(mpair? p) (cons (to-immutable (mcar p))
                      (to-immutable (mcdr p)))]
    [(vector? p)
     (let ([v (list->vector (map to-immutable (vector->list p)))])
       (if (immutable? p) (vector->immutable-vector v) v))]
    [else p]))

(define (to-mutable v)
  ; in order to handle shared structure, we need to walk the tree and build things up via mutation
  (let loop ([v v]
             [visited '()])
    (cond
      [(pair? v)
       (let ([cached (assq v visited)])
         ; if we've seen this pair before, return it
         (if cached (cdr cached)
             ; otherwise, allocate a new pair to be filled in
             (let* ([p (mcons #f #f)]
                    [visited* (cons (cons v p) visited)])
               (set-mcar! p (loop (car v) visited*))
               (set-mcdr! p (loop (cdr v) visited*))
               p)))]
      [(vector? v)
       (let ([vec (list->vector (map (λ (x) (loop x visited)) (vector->list v)))])
         (if (immutable? v) (vector->immutable-vector vec) vec))]
      [else v])))

(module+ test
  (require racket/shared
           rackunit)
  
  (check-equal? (to-mutable (shared ([p (cons 1 p)])
                              p))
                (let ([p (mcons 1 #f)])
                  (set-mcdr! p p)
                  p)))
