#lang racket

(require "utils.rkt" "schema.rkt")
(provide gen-tacky)


(define unary? (contains? 'negate 'complement 'not))
(define binary? (contains? 'add 'subtract 'multiply 'divide 'remainder
                           'and 'or ; these get removed in this pass
                           'equal 'not-equal
                           'less-than 'less-or-equal 'greater-than 'greater-or-equal
                           'lshift 'rshift
                           'bitwise-and 'bitwise-xor 'bitwise-or))

(define (ensure-schema value)
  (define expr
    (delay
      (schema-any
        (list binary? expr expr span?)
        (list unary? expr span?)
        (list 'int integer? span?))))
  (define program
    `(program (function "main" (return ,expr ,span?) ,span?) ,span?))
  (define (err bad-value schema)
    (raise-user-error 'invalid-tacky-input "~v doesn't match ~v in ~v" bad-value schema value))
  (check-schema value program err))


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
  (ensure-schema ast)
  ((bottom-up (match-lambda
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
               [x x])) ast))