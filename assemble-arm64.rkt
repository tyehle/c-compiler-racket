#lang racket

(require "utils.rkt" "schema.rkt")
(provide assemble-arm64)


;; ensure-schema : ast? -> ast?
;; Validate that value conforms to the expected arm64 assembly grammar.
;; Returns value unchanged, or raises an error if validation fails.
(define (ensure-schema value)
  (define op2 (schema-any 'neg 'mvn 'mov 'str 'ldr 'cmp))
  (define op3 (schema-any 'add 'sub 'mul 'sdiv 'lsl 'asr 'and 'orr 'eor))
  (define register (schema-any 'W0 'W9 'W10 'W11))
  (define cond-code (schema-any 'eq 'ne 'lt 'le 'gt 'ge))
  (define operand
    (schema-any
      `(reg ,register ,span?)
      `(stack ,integer? ,span?)
      `(imm ,integer? ,span?)))

  (define instruction
    (schema-any
      `(ret ,span?)
      `(,op2 ,operand ,operand ,span?)
      `(,op3 ,operand ,operand ,operand ,span?)
      `(msub ,operand ,operand ,operand ,operand ,span?)
      `(cset ,operand ,cond-code ,span?)
      `(,(schema-any 'label 'b) ,string? ,span?)
      `(,(schema-any 'cbz 'cbnz) ,operand ,string? ,span?)
      `(allocate-stack ,integer? ,span?)))

  ;; program : schema?
  ;; Schema for the lowered arm64 assembly program.
  (define program
    `(program (function "main" ,(schema-many instruction) ,span?) ,span?))

  (check-schema value program (schema-error-proc "Invalid arm assembly" value))
  value)

;; err : (or/c string? span?) string? ... -> void?
;; Raise an assembler error with a source location and formatted message.
(define (err loc . msg)
  (raise-user-error 'error "~a: assembler: ~a" (format-loc loc) (apply format msg)))

(define (cond-jump? op) (hash-has-key? cond-jump-map op))
(define cond-jump-map
  (hash
    'jump-if-zero 'cbz
    'jump-if-not-zero 'cbnz))

;; relational? : symbol? -> boolean?
;; Recognize relational comparison operators.
(define (relational? code) (hash-has-key? relational-map code))
(define relational-map
  (hash
    'equal 'eq
    'not-equal 'ne
    'less-than 'lt
    'less-or-equal 'le
    'greater-than 'gt
    'greater-or-equal 'ge))

;; unary? : symbol? -> boolean?
;; Recognize TACKY unary operators that map directly to arm64 instructions.
(define (unary? op) (hash-has-key? unary-map op))
(define unary-map
  (hash
    'negate 'neg
    'complement 'mvn))


;; binary? : symbol? -> boolean?
;; Recognize TACKY binary operators that map directly to arm64 instructions.
(define (binary? op) (hash-has-key? binary-map op))
(define binary-map
  (hash
    'add 'add
    'subtract 'sub
    'multiply 'mul
    'divide 'sdiv
    'lshift 'lsl
    'rshift 'asr
    'bitwise-and 'and
    'bitwise-or 'orr
    'bitwise-xor 'eor))


(define rewrite-operators
  (bottom-up
    (match-lambda
      ;; reverse arg order for copies for arm assembly binary tree
      [`(copy ,src ,dst ,loc)
       `(copy ,dst ,src ,loc)]
      [`(return ,op ,loc)
       `((copy (reg W0 ,loc) ,op ,loc)
         (ret ,loc))]
      [`(remainder ,a ,b ,dst ,loc)
       `((copy (reg W9 ,loc) ,a ,loc)
         (copy (reg W10 ,loc) ,b ,loc)
         (sdiv (reg W11 ,loc) (reg W9 ,loc) (reg W10 ,loc) ,loc)
         (msub (reg W9 ,loc) (reg W11 ,loc) (reg W10 ,loc) (reg W9 ,loc) ,loc)
         (copy ,dst (reg W9 ,loc) ,loc))]
      [`(,(? cond-jump? op) ,what ,where ,loc)
       `((copy (reg W9 ,loc) ,what ,loc)
         (,(hash-ref cond-jump-map op) (reg W9 ,loc) ,where ,loc))]
      [`(,(? relational? kind) ,a ,b ,dst ,loc)
       `((copy (reg W9 ,loc) ,a ,loc)
         (copy (reg W10 ,loc) ,b ,loc)
         (cmp (reg W9 ,loc) (reg W10 ,loc) ,loc)
         (cset (reg W9 ,loc) ,(hash-ref relational-map kind) ,loc)
         (copy ,dst (reg W9 ,loc) ,loc))]
      [`(jump ,where ,loc)
       `(b ,where ,loc)]
      [`(,(? unary? kind) ,src ,dst ,loc)
       `((copy (reg W9 ,loc) ,src ,loc)
         (,(hash-ref unary-map kind) (reg W9 ,loc) (reg W9 ,loc) ,loc)
         (copy ,dst (reg W9 ,loc) ,loc))]
      [`(,(? binary? kind) ,a ,b ,dst ,loc)
       `((copy (reg W9 ,loc) ,a ,loc)
         (copy (reg W10 ,loc) ,b ,loc)
         (,(hash-ref binary-map kind) (reg W9 ,loc) (reg W9 ,loc) (reg W10 ,loc) ,loc)
         (copy ,dst (reg W9 ,loc) ,loc))]
      [x x])))


;; replace-vars : any/c -> any/c
;; Replace all (var name loc) references with (stack offset loc) and
;; prepend an allocate-stack instruction to each function.
(define (replace-vars ast)
  (define stack-offset 0)
  ;; next-stack-offset : -> integer?
  ;; Allocate the next 4-byte stack slot and return its offset.
  (define (next-stack-offset)
    (set! stack-offset (+ 4 stack-offset))
    stack-offset)

  (define var-map (make-hash))

  (define transform
    (bottom-up
     (match-lambda
       [`(var ,name ,loc)
        (let ([offset (hash-ref! var-map name next-stack-offset)])
          `(stack ,offset ,loc))]
       [`(function ,name ,is ,loc)
        (let ([frame-size (+ stack-offset (modulo (- stack-offset) 16))])
          (set! stack-offset 0)
          `(function ,name ((allocate-stack ,frame-size ,loc) ,@is) ,loc))]
       [x x])))

  (transform ast))

(define (imm? node) (eq? (car node) 'imm))
(define (reg? node) (eq? (car node) 'reg))
(define (stack? node) (eq? (car node) 'stack))
(define rewrite-copies
  (bottom-up
    (match-lambda
      [`(copy ,(? reg? dst) ,(? stack? src) ,loc) `(ldr ,dst ,src ,loc)]
      [`(copy ,(? reg? dst) ,src ,loc) `(mov ,dst ,src ,loc)]
      [`(copy ,(? stack? dst) ,(? reg? src) ,loc) `(str ,src ,dst ,loc)]
      (`(copy ,(? stack? dst) ,(? imm? src) ,loc) `((mov (reg W9 ,loc) ,src ,loc)
                                                    (str (reg W9 ,loc) ,dst ,loc)))
      [`(copy ,dst ,src ,loc) (err loc "Invalid copy: ~a <- ~a" dst src)]
      [x x])))

(define (assemble-arm64 tacky)
  (ensure-schema (rewrite-copies (replace-vars (rewrite-operators tacky)))))
