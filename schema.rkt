#lang racket

(require "utils.rkt")

(provide check-schema schema-any schema-many schema-error-proc)


;; schema : name proc
;; A callable schema with a readable printed representation.
;; Invoked as (schema value err) via prop:procedure.
(struct schema (name proc)
  #:property prop:procedure (struct-field-index proc)
  #:methods gen:custom-write
  [(define (write-proc self port _mode)
     (fprintf port "#<schema:~a>" (schema-name self)))])


(define ((schema-error-proc message value) bad-value schm)
  (raise-user-error 'schema-error
                    "~a: ~v doesn't match\n~a\nin\n~a"
                    message
                    bad-value
                    (pretty-format schm)
                    (pretty-format value)))


;; check-schema : any/c schema? (any/c schema? -> void?) -> void?
;; Validate value against schema, calling err on mismatch.
;; A schema is one of:
;;   - a 1-arg predicate (e.g. string?) — value must satisfy it
;;   - a 2-arg procedure (value err -> void?) — a custom validator (e.g. from schema-any)
;;   - a list of schemas — value must be a list of the same length, checked element-wise
;;   - a literal — value must be equal? to it
;;   - a promise — forced before matching
(define (check-schema value schema err)
  ; (debug 'check value (force schema))
  (match (force schema)
    [(? procedure? schema)
     (if (= (procedure-arity schema) 1)
       (unless (schema value) (err value schema))
       (schema value err))]
    [(? list? schema)
     (cond
       [(or (not (list? value)) (not (= (length value) (length schema)))) (err value schema)]
       [else (for ([s schema] [v value]) (check-schema v s err))])]
    [schema (unless (equal? schema value) (err value schema))]))


;; check-each-schema : any/c (listof schema?) (any/c schema? -> void?) -> void?
;; Try each schema in order; backtrack on failure by replacing err with
;; an escape continuation handler that tries the next option.
(define (check-each-schema value schemas err)
  (match schemas
    ['() (err value '())]
    [(cons option options)
     (let/ec escape
       (check-schema
        value
        option
        (λ (_v _s) (escape
                    (check-each-schema value options err)))))]))


;; schema-any : schema? ... -> schema?
;; A schema that accepts a value matching any of the given schemas.
;; On failure, reports all alternatives that were tried.
(define (schema-any . schemas)
  (letrec ([s (schema `(schema-any ,@schemas)
                      (λ (value err)
                        (check-each-schema value
                                           schemas
                                           (λ (v _) (err v s)))))])
    s))


;; schema-many : schema? -> schema?
;; A schema that accepts a list where every element matches schema.
(define (schema-many inner-schema)
  (letrec ([s (schema `(schema-many ,inner-schema)
                      (λ (value err)
                        (match value
                          ['() (void)]
                          [(cons fst rst) (begin (check-schema fst inner-schema err)
                                                 (s rst err))]
                          [_ (err value s)])))])
    s))
