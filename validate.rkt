#lang racket

(require "utils.rkt")
(provide validate)


;; err : (or/c string? span?) string? ... -> void?
;; Raise a semantic analysis error with a source location and formatted message.
(define (err loc . msg)
  (raise-user-error 'error "~a: semantic analysis: ~a" (format-loc loc) (apply format msg)))

;; assign-un-op? : any/c -> boolean?
;; Recognize unary assignment operator names, including compound assignments.
(define assign-un-op?
  (contains?
    'pre-increment
    'pre-decrement
    'post-increment
    'post-decrement))

;; assign-bin-op? : any/c -> boolean?
;; Recognize binary assignment operator names, including compound assignments.
(define assign-bin-op?
  (contains?
    'assign
    'add-assign
    'sub-assign
    'mult-assign
    'div-assign
    'rem-assign
    'rshift-assign
    'lshift-assign
    'bit-and-assign
    'bit-or-assign
    'bit-xor-assign))

;; validate-labels : ast? -> ast?
;; Check that all labels in each function are unique and all goto targets exist.
(define (validate-labels ast)
  ;; labels : (hash/c string? node?)
  ;; Map from label name to its defining node, for the current function.
  (define labels (make-hash))
  ;; gotos : (hash/c string? node?)
  ;; Map from goto target name to its goto node, for the current function.
  (define gotos (make-hash))

  ;; transform : ast? -> ast?
  ;; Walk the tree, collecting labels and gotos, validating at function boundaries.
  (define transform
    (bottom-up
      (match-lambda
        [(and node `(goto ,name ,_)) (hash-set! gotos name node) node]
        [(and node `(label ,name ,_ ,loc))
         ;; ensure no duplicate labels
         (when (hash-has-key? labels name)
           (err loc "Duplicate label definition: ~a" name))
         (hash-set! labels name node)
         node]
        [(and func `(function ,_ ,_ ,_))
         ;; ensure all goto labels exist
         (for ([(name bad-goto) gotos] #:unless (hash-has-key? labels name))
           (err (last bad-goto) "Unknown label: ~a" name))
         (hash-clear! gotos)
         (hash-clear! labels)
         func]
        [x x])))

  (transform ast))

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
  ;; invalid-assign-lhs? : expr? -> boolean?
  ;; True if expr cannot appear on the left-hand side of an assignment.
  (define invalid-assign-lhs?
    (match-lambda
      [`(var ,_ ,_) #f]
      [_ #t]))
  ;; transform : ast? -> ast?
  ;; Rewrite a single AST node: mangle declarations, resolve variable
  ;; references, and reject invalid assignment targets.
  ;; Uses a single global variable map — will need scoped maps when we add
  ;; nested blocks and multiple functions.
  (define transform
    (match-lambda
      [`(declare ,name ,loc)
       `(declare ,(map-name name loc) ,loc)]
      [`(declare-init ,name ,expr ,loc)
       `(declare-init ,(map-name name loc) ,expr ,loc)]
      [`(,(? assign-un-op?) ,(? invalid-assign-lhs? lhs) ,loc)
       (err loc "Invalid assignment. Cannot assign to ~a expression" (car lhs))]
      [`(,(? assign-bin-op?) ,(? invalid-assign-lhs? lhs) ,_ ,loc)
       (err loc "Invalid assignment. Cannot assign to ~a expression" (car lhs))]
      [`(var ,name ,loc)
       `(var ,(hash-ref var-map name (λ () (err loc "Undeclared variable, ~a" name))) ,loc)]
      [x x]))
  ((top-down transform) ast))


;; validate : ast? -> ast?
;; Run all semantic analysis passes on the AST.
(define (validate ast)
  (validate-labels (resolve-vars ast)))