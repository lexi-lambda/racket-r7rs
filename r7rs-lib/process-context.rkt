#lang racket/base

(require compatibility/mlist
         (prefix-in r: racket/base))

(provide command-line exit get-environment-variables
         (rename-out [exit emergency-exit]
                     [getenv get-environment-variable]))

(define (command-line)
  (mcons (find-system-path 'run-file)
         (list->mlist (vector->list (current-command-line-arguments)))))

(define (exit [obj #t])
  (if obj (r:exit obj) (r:exit 1)))

(define (get-environment-variables)
  (let ([env (current-environment-variables)])
    (list->mlist (map (λ (name) (mcons (bytes->string/locale name)
                                       (bytes->string/locale (environment-variables-ref env name))))
                      (environment-variables-names env)))))
