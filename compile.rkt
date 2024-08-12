#lang racket

(require "lex.rkt" "parse.rkt" "tacky.rkt" "assemble.rkt" "emit.rkt")
(provide rcc-compile)


(define (rcc-compile input-file mode assembly-file)
  (let* ([tokens (delay (lex input-file))]
         [ast (delay (parse (force tokens)))]
         [tacky-ir (delay (gen-tacky (force ast)))]
         [assembly (delay (assemble (force tacky-ir)))])
    (match mode
      ['lex (pretty-print (force tokens))]
      ['parse (pretty-print (force ast))]
      ['tacky (pretty-print (force tacky-ir))]
      ['codegen (pretty-print (force assembly))]
      [(or 'assemble 'full) (emit-assembly (force assembly) assembly-file)])))


(module+ main
  (rcc-compile "programs/return_2.c" 'assemble "programs/return_2.s"))
