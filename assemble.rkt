#lang racket

(require "utils.rkt" "schema.rkt")
(provide assemble)


(define unary? (contains? 'negate 'complement))
(define standard-binary? (contains? 'add 'subtract 'multiply 'lshift 'rshift 'bitwise-and 'bitwise-xor 'bitwise-or))
(define cond-jump? (contains? 'jump-if-zero 'jump-if-not-zero))
(define relational? (contains? 'equal 'not-equal
                               'less-than 'less-or-equal
                               'greater-than 'greater-or-equal))

(define (ensure-schema value)
  (define other-binary? (contains? 'divide 'remainder))
  (define other-unary? (contains? 'not 'copy))
  (define operand
    (schema-any
      (list 'imm integer? span?)
      (list 'var string? span?)))
  (define instruction
    (schema-any
      (list unary? operand operand span?)
      (list standard-binary? operand operand operand span?)
      (list cond-jump? operand string? span?)
      (list relational? operand operand operand span?)
      (list other-unary? operand operand span?)
      (list other-binary? operand operand operand span?)
      (list 'label string? span?)
      (list 'jump string? span?)
      (list 'return operand span?)))
  (define program
    `(program (function "main" ,(schema-many instruction) ,span?) ,span?))
  (define (err bad-value schema)
    (raise-user-error 'invalid-assembler-input "~v doesn't match ~v in ~v" bad-value schema value))
  (check-schema value program err))


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


(define stack-offset 0)
(define (next-stack-offset)
  (set! stack-offset (+ 4 stack-offset))
  stack-offset)
(define var-map (make-hash))
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


(define (stack? op) (equal? 'stack (car op)))
(define (imm? op) (equal? 'imm (car op)))
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


(define (assemble tacky)
  (ensure-schema tacky)
  (fix-invalid-movs (replace-vars (rewrite-operators tacky))))
