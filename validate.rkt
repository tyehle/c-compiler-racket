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
     (λ (node)
       (match node
         [`(goto ,name ,_) (hash-set! gotos name node) node]
         [`(label ,name ,_ ,loc)
          ;; ensure no duplicate labels
          (when (hash-has-key? labels name)
            (err loc "Duplicate label definition: ~a" name))
          (hash-set! labels name node)
          node]
         [`(function ,_ ,_ ,_)
          ;; ensure all goto labels exist
          (for ([(name bad-goto) gotos] #:unless (hash-has-key? labels name))
            (err (last bad-goto) "Unknown label: ~a" name))
          (hash-clear! gotos)
          (hash-clear! labels)
          node]
         [_ node]))))

  (transform ast))

;; label-loops : ast? -> ast?
;; Assign a unique label to each loop and rewrite the placeholder #f slot on
;; every break/continue to its enclosing loop's label. Raises an error on a
;; break or continue that has no enclosing loop. Threads the current loop
;; label as the walker's context, so nested loops correctly shadow.
(define (label-loops ast)
  (define counter 0)
  ;; fresh-loop-label! : -> string?
  ;; Generate a fresh loop label.
  (define (fresh-loop-label!)
    (set! counter (+ 1 counter))
    (format "loop.v~a" counter))
  (define transform
    (contextual-top-down
     #f
     (λ (context node)
       (match node
         [`(break #f ,loc)
          (if (not context)
              (err loc "Break statement outside of loop or switch context")
              (cons context `(break ,context ,loc)))]
         [`(continue #f ,loc)
          (if (not context)
              (err loc "Continue statement outside of loop context")
              (cons context `(continue ,context ,loc)))]
         [`(while ,condition ,body #f ,loc)
          (let ([name (fresh-loop-label!)])
            (cons name `(while ,condition ,body ,name ,loc)))]
         [`(do-while ,body ,condition #f ,loc)
          (let ([name (fresh-loop-label!)])
            (cons name `(do-while ,body ,condition ,name ,loc)))]
         [`(for ,init ,control ,final ,body #f ,loc)
          (let ([name (fresh-loop-label!)])
            (cons name `(for ,init ,control ,final ,body ,name ,loc)))]
         [_ (cons context node)]))))

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

  ;; map-name! : string? span? -> string?
  ;; Register a new variable declaration in the innermost scope, returning its
  ;; mangled name. Raises an error if the name is already declared in that scope.
  (define (map-name! scope name loc)
    (if (hash-has-key? scope name)
        (err loc "Duplicate variable declaration: ~a" name)
        (let ([mangled (mangle name)])
          (hash-set! scope name mangled)
          mangled)))
  ;; lookup-name : string? span? -> string?
  ;; Resolve a source-level variable name to its mangled name by searching
  ;; from the innermost scope outward. Raises an error if no scope has a
  ;; binding for name.
  (define (lookup-name scopes name loc)
    (match scopes
      ['() (err loc "Undeclared variable: ~a" name)]
      [(cons scope outers) (hash-ref scope name (λ () (lookup-name outers name loc)))]))

  ;; invalid-assign-lhs? : expr? -> boolean?
  ;; True if expr cannot appear on the left-hand side of an assignment.
  (define invalid-assign-lhs?
    (match-lambda
      [`(var ,_ ,_) #f]
      [_ #t]))

  ;; transform : ast? -> ast?
  ;; Rewrite a single AST node: mangle declarations, resolve variable
  ;; references, and reject invalid assignment targets.
  (define transform
    (contextual-top-down
     '()
     (λ (context node)
       (match node
         [(or `(block ,@_) `(for ,@_))
          (cons (cons (make-hash) context) node)]
         [`(declare ,name ,loc)
          (cons context `(declare ,(map-name! (car context) name loc) ,loc))]
         [`(declare-init ,name ,expr ,loc)
          (cons context `(declare-init ,(map-name! (car context) name loc) ,expr ,loc))]
         [`(,(? assign-un-op?) ,(? invalid-assign-lhs? lhs) ,loc)
          (err loc "Invalid assignment. Cannot assign to ~a expression" (car lhs))]
         [`(,(? assign-bin-op?) ,(? invalid-assign-lhs? lhs) ,_ ,loc)
          (err loc "Invalid assignment. Cannot assign to ~a expression" (car lhs))]
         [`(var ,name ,loc)
          (cons context `(var ,(lookup-name context name loc) ,loc))]
         [_ (cons context node)]))))

  (transform ast))


;; validate : ast? -> ast?
;; Run all semantic analysis passes on the AST.
(define (validate ast)
  (validate-labels (label-loops (resolve-vars ast))))