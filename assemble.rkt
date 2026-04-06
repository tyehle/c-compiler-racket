#lang racket

(require "utils.rkt")
(provide assemble)


;; unary? : symbol? -> boolean?
;; Recognize TACKY unary operators that map directly to x86 instructions.
(define unary? (contains? 'negate 'complement))

;; standard-binary? : symbol? -> boolean?
;; Recognize TACKY binary operators that map to a simple two-operand x86 pattern.
(define standard-binary? (contains? 'add 'subtract 'multiply 'lshift 'rshift 'bitwise-and 'bitwise-xor 'bitwise-or))

;; cond-jump? : symbol? -> boolean?
;; Recognize conditional jump operators.
(define cond-jump? (contains? 'jump-if-zero 'jump-if-not-zero))

;; relational? : symbol? -> boolean?
;; Recognize relational comparison operators.
(define relational? (contains? 'equal 'not-equal
                               'less-than 'less-or-equal
                               'greater-than 'greater-or-equal))


;; convert-instruction-kind : symbol? -> symbol?
;; Map TACKY operator names to x86 instruction mnemonics.
(define convert-instruction-kind
  (match-lambda
    ['complement 'not]
    ['negate 'neg]
    ['add 'add]
    ['subtract 'sub]
    ['multiply 'imul]
    ['divide 'idiv]
    ['lshift 'sal]
    ['rshift 'sar]
    ['bitwise-and 'bitwise-and]
    ['bitwise-xor 'bitwise-xor]
    ['bitwise-or 'bitwise-or]))


;; rewrite-operators : any/c -> any/c
;; Lower TACKY instructions into x86 assembly instructions.
;; Multi-instruction TACKY ops (divide, remainder, conditionals) are
;; expanded into sequences; simple ops become mov+op pairs.
(define rewrite-operators
  (bottom-up (match-lambda
               [`(return ,op ,loc)
                `((mov ,op (reg AX ,loc) ,loc)
                  (ret, loc))]
               [`(divide ,a ,b ,dst ,loc)
                `((mov ,a (reg AX ,loc) ,loc)
                  (cdq ,loc)
                  (idiv ,b ,loc)
                  (mov (reg AX ,loc) ,dst ,loc))]
               [`(remainder ,a ,b ,dst ,loc)
                `((mov ,a (reg AX ,loc) ,loc)
                  (cdq ,loc)
                  (idiv ,b ,loc)
                  (mov (reg DX ,loc) ,dst ,loc))]
               [`(,(? cond-jump? kind) ,what ,where ,loc)
                `((cmp (imm 0 ,loc) ,what ,loc)
                  (jmp-cc ,kind ,where ,loc))]
               [`(,(? relational? kind) ,left ,right ,dst ,loc)
                `((cmp ,right ,left ,loc)
                  (mov (imm 0 ,loc) ,dst ,loc)
                  (set-cc ,kind ,dst ,loc))]
               [`(not ,src ,dst ,loc)
                `((cmp (imm 0 ,loc) ,src ,loc)
                  (mov (imm 0 ,loc) ,dst ,loc)
                  (set-cc equal ,dst ,loc))]
               [`(jump ,where ,loc)
                `(jmp ,where ,loc)]
               [`(copy ,src ,dst ,loc)
                `(mov ,src ,dst ,loc)]
               [`(,(? unary? kind) ,src ,dst ,loc)
                `((mov ,src ,dst ,loc)
                  (,(convert-instruction-kind kind) ,dst ,loc))]
               [`(,(? standard-binary? kind) ,a ,b ,dst ,loc)
                `((mov ,a ,dst ,loc)
                  (,(convert-instruction-kind kind) ,b ,dst ,loc))]
               [x x])))


;; Module-level state for stack allocation.
(define stack-offset 0)

;; next-stack-offset : -> integer?
;; Allocate the next 4-byte stack slot and return its offset.
(define (next-stack-offset)
  (set! stack-offset (+ 4 stack-offset))
  stack-offset)

(define var-map (make-hash))

;; replace-vars : any/c -> any/c
;; Replace all (var name loc) references with (stack offset loc) and
;; prepend an allocate-stack instruction to each function.
(define replace-vars
  (bottom-up (match-lambda
               [`(var ,name ,loc)
                (let ([offset (hash-ref! var-map name next-stack-offset)])
                  `(stack ,offset ,loc))]
               [`(function ,name ,is ,loc)
                (let ([final-offset stack-offset])
                  (set! stack-offset 0)
                  `(function ,name ((allocate-stack ,final-offset ,loc) ,@is) ,loc))]
               [x x])))


;; stack? : any/c -> boolean?
;; True if the operand is a stack reference.
(define (stack? op) (equal? 'stack (car op)))

;; imm? : any/c -> boolean?
;; True if the operand is an immediate value.
(define (imm? op) (equal? 'imm (car op)))

;; fix-invalid-movs : any/c -> any/c
;; Rewrite instructions that violate x86 encoding constraints
;; (e.g. memory-to-memory moves, immediates where not allowed)
;; by routing through scratch registers R10, R11, or CX.
(define fix-invalid-movs
  (bottom-up (match-lambda
               ; idiv cannot operate on a constant
               [`(idiv ,(and val `(imm ,_ ,_)) ,loc)
                `((mov ,val (reg R10 ,loc) ,loc)
                  (idiv (reg R10 ,loc) ,loc))]
               ; imul can't use an address as its destination
               [`(imul ,src ,(? stack? dst) ,loc)
                `((mov ,dst (reg R11 ,loc) ,loc)
                  (imul ,src (reg R11 ,loc) ,loc)
                  (mov (reg R11 ,loc) ,dst ,loc))]
               ; second arg to cmp can't be a constant
               [`(cmp ,src ,(? imm? dst) ,loc)
                `((mov ,dst (reg R11 ,loc) ,loc)
                  (cmp ,src (reg R11 ,loc) ,loc))]
               ; shift op count arg cannot be an address & the reg it operates on must be %cl
               [`(,(? (contains? 'sar 'sal) kind) ,(? stack? how-many) ,what ,loc)
                `((mov ,how-many (reg CX ,loc) ,loc)
                  (,kind (reg CX 1 ,loc) ,what ,loc))]
               ; mov, add, sub, and, or, and xor cannot operate on two addresses
               [`(,(? (contains? 'mov 'add 'sub 'cmp 'bitwise-and 'bitwise-or 'bitwise-xor) kind) ,(? stack? src) ,(? stack? dst) ,loc)
                `((mov ,src (reg R10 ,loc) ,loc)
                  (,kind (reg R10 ,loc) ,dst ,loc))]
               [x x])))


;; assemble : tacky-program? -> asm-program?
;; Lower TACKY IR to x86 assembly by rewriting operators, allocating
;; stack slots for variables, and fixing invalid instruction encodings.
(define (assemble tacky)
  (fix-invalid-movs (replace-vars (rewrite-operators tacky))))
