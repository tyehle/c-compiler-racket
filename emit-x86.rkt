#lang racket

(provide emit-x86)


;; operand->string : operand -> string
;; Convert an assembly operand node to its x86 string representation.
(define operand->string
  (match-lambda
    ; typed register. There is only one for now so we can support shifts
    [`(reg CX 1 ,_) "%cl"]
    ; untyped registers
    [`(reg AX ,_) "%eax"]
    [`(reg CX ,_) "%ecx"]
    [`(reg DX ,_) "%edx"]
    [`(reg R10 ,_) "%r10d"]
    [`(reg R11 ,_) "%r11d"]
    [`(stack ,n ,_) (format "-~a(%rbp)" n)]
    [`(imm ,value ,_) (format "$~a" value)]))


;; cond-jump-map : (hash/c symbol? string?)
;; Map from conditional jump IR names to x86 jump mnemonics.
(define cond-jump-map
  (hash 'jump-if-zero "je"
        'jump-if-not-zero "jne"))


;; comp-code-map : (hash/c symbol? string?)
;; Map from comparison IR names to x86 condition code suffixes.
(define comp-code-map
  (hash 'equal "e"
        'not-equal "ne"
        'less-than "l"
        'less-or-equal "le"
        'greater-than "g"
        'greater-or-equal "ge"))


;; label->string : string? -> string?
;; Prefix a label name to form its assembly representation.
(define (label->string name)
  (format "L~a" name))


;; unary-map : (hash/c symbol? string?)
;; Map from unary IR operation names to x86 mnemonics.
(define unary-map
  (hash 'neg "negl"
        'not "notl"
        'idiv "idivl"))
;; unary? : any/c -> boolean?
;; Recognize unary assembly operation names.
(define (unary? i) (dict-has-key? unary-map i))


;; binary-map : (hash/c symbol? string?)
;; Map from binary IR operation names to x86 mnemonics.
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
;; binary? : any/c -> boolean?
;; Recognize binary assembly operation names.
(define (binary? i) (dict-has-key? binary-map i))


;; emit-binop : string? string? string? -> void?
;; Print a two-operand x86 instruction line.
(define (emit-binop op a b)
  (printf "    ~a ~a, ~a\n" op a b))


;; emit-unop : string? string? -> void?
;; Print a one-operand x86 instruction line.
(define (emit-unop op a)
  (printf "    ~a ~a\n" op a))


;; emit-label : string? -> void?
;; Print a label definition line.
(define (emit-label name)
  (printf "~a:\n" (label->string name)))


;; emit : assembly-node -> void?
;; Recursively emit x86 text for an assembly AST node.
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


;; emit-x86 : assembly-program path-string? -> void?
;; Validate and write the assembly AST as x86 text to output-file.
(define (emit-x86 ast output-file)
  (with-output-to-file output-file (λ () (emit ast)) #:exists 'replace))
