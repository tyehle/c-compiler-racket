#lang racket

(require "utils.rkt" "schema.rkt")
(provide emit-assembly)



(define (ensure-schema value)
  (define operand
    (schema-any
      (list 'reg symbol? span?)
      (list 'reg symbol? integer? span?)
      (list 'stack integer? span?)
      (list 'imm integer? span?)))
  (define (cond-jump? cj) (dict-has-key? cond-jump-map cj))
  (define (comp-code? cc) (dict-has-key? comp-code-map cc))
  (define instruction
    (schema-any
      (list binary? operand operand span?)
      (list unary? operand span?)
      (list 'allocate-stack integer? span?)
      (list 'ret span?)
      (list 'cdq span?)
      (list 'jmp string? span?)
      (list 'jmp-cc cond-jump? string? span?)
      (list 'set-cc comp-code? operand span?)
      (list 'label string? span?)))
  (define program
    `(program (function "main" ,(schema-many instruction) ,span?) ,span?))
  (define (err bad-value schema)
    (raise-user-error 'invalid-assembly "~v doesn't match ~v in ~v" bad-value schema value))
  (check-schema value program err))


(define operand->string
  (match-lambda
    ; typed register. There is only one for now so we can support shifts
    [`(reg CX 1 ,_) "%cl"]
    ; untyped registers
    [`(reg AX ,_) "%eax"]
    [`(reg CX, _) "%ecx"]
    [`(reg DX ,_) "%edx"]
    [`(reg R10 ,_) "%r10d"]
    [`(reg R11 ,_) "%r11d"]
    [`(stack ,n ,_) (format "-~a(%rbp)" n)]
    [`(imm ,value ,_) (format "$~a" value)]))


(define cond-jump-map
  (hash 'jump-if-zero "je"
        'jump-if-not-zero "jne"))


(define comp-code-map
  (hash 'equal "e"
        'not-equal "ne"
        'less-than "l"
        'less-or-equal "le"
        'greater-than "g"
        'greater-or-equal "ge"))


(define (label->string name)
  (format "L~a" name))


(define unary-map
  (hash 'neg "negl"
        'not "notl"
        'idiv "idivl"))
(define (unary? i) (dict-has-key? unary-map i))


(define binary-map
  (hash 'mov "movl"
        'add "addl"
        'sub "subl"
        'imul "imull"
        'sar "sarl"
        'sal "sall"
        'cmp "cmpl"
        'bitwise-and "andl"
        'bitwise-xor "xorl"
        'bitwise-or "orl"))
(define (binary? i) (dict-has-key? binary-map i))


(define (emit-binop op a b)
  (printf "    ~a ~a, ~a\n" op a b))


(define (emit-unop op a)
  (printf "    ~a ~a\n" op a))


(define (emit-label name)
  (printf "~a:\n" (label->string name)))


(define (emit ast)
  (match ast
    [`(program ,fn ,_) (emit fn)]

    [`(function ,src-name ,body ,_)
     (let ([name (string-append "_" src-name)])
       (printf "    .globl ~a\n~a:\n" name name)
       (emit-unop "pushq" "%rbp")
       (emit-binop "movq" "%rsp" "%rbp")
       (for ([i body])
         (emit i)))]

    [`(allocate-stack ,n ,_)
     (emit-binop "subq" (format "$~a" n) "%rsp")]
    [`(ret ,_)
     (emit-binop "movq" "%rbp" "%rsp")
     (emit-unop "popq" "%rbp")
     (display "    ret\n")]
    [`(cdq ,_)
     (display "    cdq\n")]
    [`(jmp ,where ,_) (emit-unop "jmp" (label->string where))]
    [`(jmp-cc ,pred ,where ,_)
     (emit-unop (dict-ref cond-jump-map pred) (label->string where))]
    [`(set-cc ,cc ,what ,_)
     (emit-unop (format "set~a" (dict-ref comp-code-map cc)) (operand->string what))]
    [`(label ,name ,_) (emit-label name)]
    [`(,(? binary? op) ,src ,dst ,_)
     (emit-binop (dict-ref binary-map op) (operand->string src) (operand->string dst))]
    [`(,(? unary? op) ,value ,_)
     (emit-unop (dict-ref unary-map op) (operand->string value))]))


(define (emit-assembly ast output-file)
  (ensure-schema ast)
  (with-output-to-file output-file (λ () (emit ast)) #:exists 'replace))
