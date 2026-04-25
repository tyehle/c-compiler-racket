#lang racket

(require "utils.rkt" "schema.rkt")
(provide gen-tacky)


;; ensure-schema : any/c -> any/c
;; Validate that value is a well-formed TACKY IR program.
;; Returns value unchanged, or raises an error if validation fails.
(define (ensure-schema value)
  ;; unary : schema?
  ;; Unary instruction operators.
  (define unary (schema-any 'negate 'complement 'not 'copy))
  ;; binary : schema?
  ;; Binary instruction operators.
  (define binary (schema-any 'add 'subtract 'multiply 'divide 'remainder
                              'lshift 'rshift 'bitwise-and 'bitwise-xor 'bitwise-or
                              'equal 'not-equal
                              'less-than 'less-or-equal 'greater-than 'greater-or-equal))
  ;; cond-jump : schema?
  ;; Conditional jump operators.
  (define cond-jump (schema-any 'jump-if-zero 'jump-if-not-zero))
  ;; operand : schema?
  ;; An immediate value or a variable reference.
  (define operand
    (schema-any
      (list 'imm integer? span?)
      (list 'var string? span?)))
  ;; instruction : schema?
  ;; A single TACKY instruction.
  (define instruction
    (schema-any
      (list unary operand operand span?)
      (list binary operand operand operand span?)
      (list cond-jump operand string? span?)
      (list (schema-any 'label 'jump) string? span?)
      (list 'return operand span?)))
  ;; program : schema?
  ;; Schema for the top-level TACKY IR program.
  (define program
    `(program (function "main" ,(schema-many instruction) ,span?) ,span?))
  (check-schema value program (schema-error-proc "Invalid AST" value))
  value)


;; unary? : symbol? -> boolean?
;; Recognize unary operators in the parse AST.
(define unary? (contains? 'negate 'complement 'not))
;; binary? : symbol? -> boolean?
;; Recognize binary operators in the parse AST.
(define binary? (contains? 'add 'subtract 'multiply 'divide 'remainder
                           'and 'or ; these get removed in this pass
                           'equal 'not-equal
                           'less-than 'less-or-equal 'greater-than 'greater-or-equal
                           'lshift 'rshift
                           'bitwise-and 'bitwise-xor 'bitwise-or))

;; assign-op-map : (hash/c symbol? symbol?)
;; Map from compound assignment operator names to their underlying binary operators.
(define assign-op-map
  (hash
    'add-assign     'add
    'sub-assign     'subtract
    'mult-assign    'multiply
    'div-assign     'divide
    'rem-assign     'remainder
    'rshift-assign  'rshift
    'lshift-assign  'lshift
    'bit-and-assign 'bitwise-and
    'bit-or-assign  'bitwise-or
    'bit-xor-assign 'bitwise-xor))


;; Monotonic counter shared by fresh-tacky-tmp-var and fresh-tacky-label.
(define next-tacky-var 0)

;; fresh-tacky-tmp-var : span? -> operand?
;; Generate a fresh temporary variable operand.
(define (fresh-tacky-tmp-var loc)
  (let ([name (format "fresh.t~a" next-tacky-var)])
    (set! next-tacky-var (add1 next-tacky-var))
    `(var ,name ,loc)))

;; fresh-tacky-label : symbol? -> string?
;; Generate a fresh label name with the given hint prefix.
(define (fresh-tacky-label hint)
  (let [(name (format "~a_~a" hint next-tacky-var))]
    (set! next-tacky-var (add1 next-tacky-var))
    name))


;; gen-tacky : ast? -> tacky-program?
;; Lower a validated parse AST into TACKY IR using a bottom-up tree walk.
;; The transform rewrites nodes in two conventions:
;;   - Statements become flat lists of instructions (in forward order).
;;   - Expressions become (cons result-operand reversed-instructions),
;;     where instructions are accumulated in reverse and reversed at
;;     the statement boundary.
;; Short-circuit operators (and/or) are lowered to conditional jumps.
(define (gen-tacky ast)
  ;; desugar : ast? -> ast?
  ;; Rewrite pre-increment/decrement into compound assignment forms.
  (define desugar
    (top-down
      (match-lambda
        [`(pre-increment ,what ,loc) `(add-assign ,what (int 1 ,loc) ,loc)]
        [`(pre-decrement ,what ,loc) `(sub-assign ,what (int 1 ,loc) ,loc)]
        [x x])))

  ;; transform : any/c -> any/c
  ;; Rewrite a single AST node into TACKY IR.
  (define transform
    (bottom-up
     (match-lambda
       ;; statements (all of these become lists of instructions)
       [`(declare ,_ ,_)
        '()]
       [`(declare-init ,name (,rhs ,@instructions) ,loc)
        (reverse
         `((copy ,rhs (var ,name ,loc) ,loc)
           ,@instructions))]
       [`(return (,v ,@instructions) ,loc)
        (reverse (cons `(return ,v ,loc) instructions))]
       [`(expression (,_ ,@instructions) ,_)
        (reverse instructions)]
       [`(null ,_)
        '()]
       ;; expressions (all of these become (cons var instruction-list))
       [`(var ,name ,loc) (list `(var ,name ,loc))]
       [`(int ,n ,loc) (list `(imm ,n ,loc))]
       [`(,(and (or 'post-increment 'post-decrement) kind) (,lhs-val ,@lhs) ,loc)
        (let ([tmp-val (fresh-tacky-tmp-var loc)]
              [op (if (eq? kind 'post-increment) 'add 'subtract)])
          `(,tmp-val
            (,op ,lhs-val (imm 1 ,loc) ,lhs-val ,loc)
            (copy ,lhs-val ,tmp-val ,loc)
            ,@lhs))]
       [`(assign (,lhs-val ,@lhs) (,rhs-val ,@rhs) ,loc)
        `(,lhs-val
          (copy ,rhs-val ,lhs-val ,loc)
          ,@rhs
          ,@lhs)]
       [`(,(? (curry hash-has-key? assign-op-map) kind) (,lhs-val ,@lhs) (,rhs-val ,@rhs) ,loc)
        `(,lhs-val
          (,(hash-ref assign-op-map kind) ,lhs-val ,rhs-val ,lhs-val ,loc)
          ,@rhs
          ,@lhs)]
       [`(and (,fst-val ,@fst) (,snd-val ,@snd) ,loc)
        (let [(result (fresh-tacky-tmp-var loc))
              (false-label (fresh-tacky-label 'and_false))
              (end-label (fresh-tacky-label 'and_end))]
          `(,result
            (label ,end-label ,loc)
            (copy (imm 0 ,loc) ,result ,loc)
            (label ,false-label ,loc)
            (jump ,end-label ,loc)
            (copy (imm 1 ,loc) ,result ,loc)
            (jump-if-zero ,snd-val ,false-label ,loc)
            ,@snd
            (jump-if-zero ,fst-val ,false-label ,loc)
            ,@fst))]
       [`(or (,fst-val ,@fst) (,snd-val ,@snd) ,loc)
        (let [(result (fresh-tacky-tmp-var loc))
              (true-label (fresh-tacky-label 'or_true))
              (end-label (fresh-tacky-label 'or_end))]
          `(,result
            (label ,end-label ,loc)
            (copy (imm 1 ,loc) ,result ,loc)
            (label ,true-label ,loc)
            (jump ,end-label ,loc)
            (copy (imm 0 ,loc) ,result ,loc)
            (jump-if-not-zero ,snd-val ,true-label ,loc)
            ,@snd
            (jump-if-not-zero ,fst-val ,true-label ,loc)
            ,@fst))]
       [`(,(? unary? kind) (,operand ,@instructions) ,loc)
        (let* ([dest (fresh-tacky-tmp-var loc)]
               [instr `(,kind ,operand ,dest ,loc)])
          (cons dest (cons instr instructions)))]
       [`(,(? binary? kind) (,a ,@a-instrs) (,b ,@b-instrs) ,loc)
        (let* ([dest (fresh-tacky-tmp-var loc)]
               [instr `(,kind ,a ,b ,dest ,loc)])
          (cons dest (cons instr (append a-instrs b-instrs))))]
       [x x])))
  (ensure-schema (transform (desugar ast))))