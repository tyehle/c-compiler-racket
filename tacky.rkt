#lang racket

(require "utils.rkt" "schema.rkt")
(provide gen-tacky)


(define (ensure-schema value)
  (define unary? (contains? 'negate 'complement 'not 'copy))
  (define binary? (contains? 'add 'subtract 'multiply 'divide 'remainder
                             'lshift 'rshift 'bitwise-and 'bitwise-xor 'bitwise-or
                             'equal 'not-equal
                             'less-than 'less-or-equal 'greater-than 'greater-or-equal))
  (define cond-jump? (contains? 'jump-if-zero 'jump-if-not-zero))
  (define operand
    (schema-any
      (list 'imm integer? span?)
      (list 'var string? span?)))
  (define instruction
    (schema-any
      (list unary? operand operand span?)
      (list binary? operand operand operand span?)
      (list cond-jump? operand string? span?)
      (list (contains? 'label 'jump) string? span?)
      (list 'return operand span?)))
  (define program
    `(program (function "main" ,(schema-many instruction) ,span?) ,span?))
  (define (err bad-value schema)
    (raise-user-error 'error "Invalid AST: ~v doesn't match ~v in ~v" bad-value schema value))
  (check-schema value program err)
  value)


(define unary? (contains? 'negate 'complement 'not))
(define binary? (contains? 'add 'subtract 'multiply 'divide 'remainder
                           'and 'or ; these get removed in this pass
                           'equal 'not-equal
                           'less-than 'less-or-equal 'greater-than 'greater-or-equal
                           'lshift 'rshift
                           'bitwise-and 'bitwise-xor 'bitwise-or))


(define next-tacky-var 0)
(define (fresh-tacky-tmp-var loc)
  (let ([name (format "tmp.~a" next-tacky-var)])
    (set! next-tacky-var (add1 next-tacky-var))
    `(var ,name ,loc)))
(define (fresh-tacky-label hint)
  (let [(name (format "~a_~a" hint next-tacky-var))]
    (set! next-tacky-var (add1 next-tacky-var))
    name))


(define (gen-tacky ast)
  (define transform
    (match-lambda
      [`(return (,v ,@instructions) ,loc)
       (reverse (cons `(return ,v ,loc) instructions))]
      [`(int ,n ,loc) (list `(imm ,n ,loc))]
      [`(and (,fst-val ,@fst) (,snd-val ,@snd) ,loc)
       (let [(result (fresh-tacky-tmp-var loc))
             (false-label (fresh-tacky-label 'and_false))
             (end-label (fresh-tacky-label 'and_end))]
         `(,result
           (label ,end-label ,loc)
           (copy (imm 0 ,loc) ,result, loc)
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
           (copy (imm 1 ,loc) ,result, loc)
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
      [x x]))
  (ensure-schema ((bottom-up transform) ast)))