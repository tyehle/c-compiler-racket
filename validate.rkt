#lang racket

(require "utils.rkt")
(provide validate)


;; err : (or/c string? span?) string? ... -> void?
;; Raise a semantic analysis error with a source location and formatted message.
(define (err loc . msg)
  (raise-user-error 'error "~a: semantic analysis: ~a" (format-loc loc) (apply format msg)))


;; resolve-vars : ast? -> ast?
;; Resolve variable references and rename all variables to unique names.
;; Checks for duplicate declarations and undeclared variable uses.
(define (resolve-vars ast)
  (define counter 0)
  ;; mangle : string? -> string?
  ;; Generate a unique name by appending a monotonic suffix.
  (define (mangle name)
    (set! counter (+ 1 counter))
    (format "~a.v~a" name counter))
  (define var-map (make-hash))
  ;; map-name : string? span? -> string?
  ;; Register a new variable declaration, returning its mangled name.
  ;; Raises an error if the variable is already declared.
  (define (map-name name loc)
    (if (hash-has-key? var-map name)
        (err loc "Duplicate variable declaration: ~a" name)
        (let [(mangled (mangle name))]
          (hash-set! var-map name mangled)
          mangled)))
  ;; transform : ast? -> ast?
  ;; Rewrite a single AST node: mangle declarations, resolve variable
  ;; references, and reject invalid assignment targets.
  ;; Uses a single global variable map — will need scoped maps when we add
  ;; nested blocks and multiple functions.
  (define transform
    (match-lambda
      [`(declare ,name ,loc) `(declare ,(map-name name loc) ,loc)]
      [`(declare-init ,name ,expr ,loc) `(declare-init ,(map-name name loc) ,expr ,loc)]
      [`(assign (,kind ,@_ ,loc) ,_ ,_) #:when (not (eq? kind 'var)) (err loc "Invalid assignment. Cannot assign to a ~a expression" kind)]
      [`(var ,name ,loc) `(var ,(hash-ref var-map name (λ () (err loc "Undeclared variable, ~a" name))) ,loc)]
      [x x]))
  ((top-down transform) ast))


;; validate : ast? -> ast?
;; Run all semantic analysis passes on the AST.
(define (validate ast)
  (resolve-vars ast))