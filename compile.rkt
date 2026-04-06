#lang racket

(require "lex.rkt" "parse.rkt" "validate.rkt" "tacky.rkt" "assemble.rkt" "emit.rkt")
(provide rcc-compile)


;; rcc-compile : path-string? symbol? path-string? -> void?
;; Run the compiler pipeline up to the given mode, printing or emitting output.
(define (rcc-compile input-file mode assembly-file)
  (let* ([tokens (delay (lex input-file))]
         [ast (delay (parse (force tokens)))]
         [validated (delay (validate (force ast)))]
         [tacky-ir (delay (gen-tacky (force validated)))]
         [assembly (delay (assemble (force tacky-ir)))])
    (match mode
      ['lex (pretty-print (force tokens))]
      ['parse (pretty-print (force ast))]
      ['validate (pretty-print (force validated))]
      ['tacky (pretty-print (force tacky-ir))]
      ['codegen (pretty-print (force assembly))]
      [(or 'assemble 'full) (emit-assembly (force assembly) assembly-file)])))


(module+ main
  (rcc-compile "programs/return_2.c" 'assemble "programs/return_2.s"))
