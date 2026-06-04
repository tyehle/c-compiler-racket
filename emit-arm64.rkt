#lang racket

(require racket/format)

(provide emit-arm64)

;; operand->string : operand -> string
;; Convert an assembly operand node to its x86 string representation.
(define operand->string
  (match-lambda
    ; untyped registers
    [`(reg W0 ,_) "w0"]
    [`(reg W9 ,_) "w9"]
    [`(reg W10 ,_) "w10"]
    [`(reg W11 ,_) "w11"]
    [`(stack ,n ,_) (format "[x29, #-~a]" n)]
    [`(imm ,value ,_) (format "#~a" value)]))

(define (emit-op . args)
  (printf "        ~a~a\n" (~a (car args) #:min-width 8) (apply ~a #:separator ", " (cdr args))))

(define emit
  (match-lambda
    [`(program ,fn ,_) (emit fn)]

    [`(function ,src-name ,body ,_)
     (let ([name (string-append "_" src-name)])
       (printf "        .globl ~a\n" name)
       (printf "~a:\n" name)
       (emit-op "stp" "x29" "x30" "[sp, #-16]!")
       (emit-op "mov" "x29" "sp")
       (for ([i body])
         (emit i)))]

    [`(allocate-stack ,n ,_)
     (emit-op "sub" "sp" "sp" (format "#~a" n))]
    [`(ret ,_)
     (emit-op 'mov "sp" "x29")
     (emit-op 'ldp "x29" "x30" "[sp]" "#16")
     (emit-op 'ret)]
    [`(,op ,dst ,src ,_)
     (emit-op op (operand->string dst) (operand->string src))]
    [`(,op ,dst ,a ,b ,_)
     (emit-op op (operand->string dst) (operand->string a) (operand->string b))]
    [`(,op ,dst ,a ,b ,c ,_)
     (emit-op op (operand->string dst) (operand->string a) (operand->string b) (operand->string c))]
    [x x]))

;; emit-arm64 : assembly-program path-string? -> void?
;; Validate and write the assembly AST as x86 text to output-file.
(define (emit-arm64 ast output-file)
  (with-output-to-file output-file (λ () (emit ast)) #:exists 'replace))