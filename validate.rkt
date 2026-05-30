#lang racket

(require "utils.rkt" "schema.rkt")
(provide validate)

;; ensure-schema : ast? -> ast?
;; Validate that value conforms to the expected parse tree grammar.
;; Returns value unchanged, or raises an error if validation fails.
(define (ensure-schema value)
  ;; Recognize unary operator names.
  (define unary (schema-any 'negate 'complement 'not
                            'pre-increment 'pre-decrement 'post-increment 'post-decrement))
  ;; Recognize binary operator names.
  (define binary (schema-any 'add 'subtract 'multiply 'divide 'remainder
                             'and 'or ; these get removed in the next pass
                             'equal 'not-equal
                             'less-than 'less-or-equal 'greater-than 'greater-or-equal
                             'lshift 'rshift
                             'bitwise-and 'bitwise-xor 'bitwise-or
                             'assign
                             'add-assign 'sub-assign 'mult-assign 'div-assign 'rem-assign
                             'rshift-assign 'lshift-assign
                             'bit-and-assign 'bit-or-assign 'bit-xor-assign))
  ;; Schema for expression nodes (delayed for self-reference).
  (define expr
    (delay
      (schema-any
        (list binary expr expr span?)
        (list unary expr span?)
        (list 'conditional expr expr expr span?)
        (list 'var string? span?)
        (list 'int integer? span?))))
  ;; Schema for statements
  (define statement
    (delay
      (schema-any
       `(return ,expr ,span?)
       `(expression ,expr ,span?)
       `(if ,expr ,statement ,span?)
       `(if-else ,expr ,statement ,statement ,span?)
       `(compound ,block ,span?)
       `(break ,string? ,span?)
       `(continue ,string? ,span?)
       `(while ,expr ,statement ,string? ,span?)
       `(do-while ,statement ,expr ,string? ,span?)
       `(for ,block-item ,expr ,statement ,statement ,string? ,span?)
       `(switch ,expr ,statement ,string? ,hash? ,span?)
       `(case ,statement ,string? ,span?)
       `(null ,span?)
       `(goto ,string? ,span?)
       `(label ,string? ,statement ,span?))))
  ;; Schema for declarations
  (define declaration
    (schema-any
     `(declare ,string? ,span?)
     `(declare-init ,string? ,expr ,span?)))
  ;; for loop init statement
  (define block-item (schema-any declaration statement))
  ;; Schema for blocks: a sequence of declarations and statements.
  (define block
    `(block ,(schema-many block-item)
            ,span?))
  ;; Schema for the top-level program node.
  (define program
    `(program (function "main" ,block ,span?) ,span?))

  (check-schema value program (schema-error-proc "Invalid parse tree after validation" value))
  value)

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

;; label-loop-switch : ast? -> ast?
;; Attach a unique label to each loop, switch, case, and default. Rewrite each
;; break/continue to its enclosing target — continue resolves to the enclosing
;; *loop* (not switch), break to the nearest loop or switch. Each switch also
;; collects its case/default labels into a hash that lands on the switch node.
;; Errors on break/continue/case/default outside the appropriate context, and
;; on duplicate case values or duplicate default within a single switch.
(define (label-loop-switch ast)
  ; keep track of
  ; - where to break to
  ; - where to continue to
  ; - a box to put all the cases we encounter
  (struct context (break continue cases))
  (define counter 0)
  ;; fresh-label! : symbol? -> string?
  ;; Generate a fresh label of the given kind (e.g. 'loop, 'case, 'switch).
  (define (fresh-label! kind)
    (set! counter (+ 1 counter))
    (format "~a.v~a" kind counter))

  (define transform
    (contextual-top-down
     (context #f #f #f)
     (λ (ctx node)
       (match node
         [`(break ,loc)
          (if (not (context-break ctx))
              (err loc "Break statement outside of loop or switch context")
              (cons ctx `(break ,(context-break ctx) ,loc)))]
         [`(continue ,loc)
          (if (not (context-continue ctx))
              (err loc "Continue statement outside of loop context")
              (cons ctx `(continue ,(context-continue ctx) ,loc)))]
         [`(case (int ,n ,_) ,body ,loc)
          (cond
            [(not (context-cases ctx)) (err loc "Case statement outside of switch context")]
            [(hash-has-key? (context-cases ctx) n) (err loc "Duplicate case value: ~a" n)]
            [else (let ([name (fresh-label! 'case)])
                    (hash-set! (context-cases ctx) n name)
                    (cons ctx `(case ,body ,name ,loc)))])]
         [`(case ,value ,_ ,loc) (err loc "Non-constant case value: ~a" value)]
         [`(default ,body ,loc)
          (cond
            [(not (context-cases ctx)) (err loc "Default statement outside of switch context")]
            [(hash-has-key? (context-cases ctx) 'default) (err loc "Duplicate default statement")]
            [else (let ([name (fresh-label! 'default)])
                    (hash-set! (context-cases ctx) 'default name)
                    (cons ctx `(case ,body ,name ,loc)))])]
         [`(while ,condition ,body ,loc)
          (let* ([name (fresh-label! 'loop)]
                 [new-ctx (struct-copy context ctx [break name] [continue name])])
            (cons new-ctx `(while ,condition ,body ,name ,loc)))]
         [`(do-while ,body ,condition ,loc)
          (let* ([name (fresh-label! 'loop)]
                 [new-ctx (struct-copy context ctx [break name] [continue name])])
            (cons new-ctx `(do-while ,body ,condition ,name ,loc)))]
         [`(for ,init ,control ,final ,body ,loc)
          (let* ([name (fresh-label! 'loop)]
                 [new-ctx (struct-copy context ctx [break name] [continue name])])
            (cons new-ctx `(for ,init ,control ,final ,body ,name ,loc)))]
         [`(switch ,what ,body ,loc)
          (let* ([table (make-hash)]
                 [name (fresh-label! 'switch)]
                 [new-ctx (struct-copy context ctx [break name] [cases table])])
            (cons new-ctx `(switch ,what ,body ,name ,table ,loc)))]
         [_ (cons ctx node)]))))

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
  (ensure-schema (validate-labels (label-loop-switch (resolve-vars ast)))))