#lang racket

(provide check-schema schema-any schema-many)


(define (check-schema value schema err)
  ;(debug 'check value (force schema))
  (match (force schema)
    [(? procedure? schema)
     (if (equal? (procedure-arity schema) 1)
       (if (schema value) (void) (err value schema))
       (schema value err))]
    [(? list? schema)
     (cond
       [(or (not (list? value)) (not (equal? (length value) (length schema)))) (err value schema)]
       [else (for ([s schema] [v value]) (check-schema v s err))])]
    [schema (if (equal? schema value) (void) (err value schema))]))


(define (check-each-schema value schemas err)
  (match schemas
    ['() (err value '())]
    [(cons option options)
     (let ([backtrack-err (lambda (_v _s) (check-each-schema value options err))])
       (check-schema value option backtrack-err))]))


(define (schema-any . schemas)
  (lambda (value err)
    (check-each-schema value schemas err)))


(define (schema-many schema)
  (lambda (value err)
    (match value
      ['() (void)]
      [(cons fst rst) (begin (check-schema fst schema err) ((schema-many schema) rst err))]
      [_ (err value `(schema-many ,schema))])))
