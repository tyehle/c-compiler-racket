#!/usr/bin/env racket
#lang racket

(require "compile.rkt")

;; check-valid : string? any/c (listof any/c) -> any/c
;; Return value unchanged if it appears in choices; otherwise raise a
;; user error naming the offending flag and listing the allowed values.
(define (check-valid name value choices)
  (unless (member value choices)
    (raise-user-error 'invalid-arg
                      "Invalid parameter value: ~a expected one of ~a, but got ~a" name choices value))
  value)

;; mode : (parameter/c symbol?)
;; The compiler stage to stop at; defaults to a full compile.
(define mode (make-parameter 'full))
;; target : (parameter/c symbol?)
;; The architecture to target; defaults to a x86.
(define target (make-parameter 'x86))
;; input-file : (parameter/c string?)
;; Path to the C source file to compile.
(define input-file (make-parameter #f))

;; parse-args : -> void?
;; Parse command-line flags and set the mode and input-file parameters.
(define (parse-args)
  (command-line
    #:program "rcc"
    #:once-any
    [("--lex")
      "Stop after lexing; print the token stream"
      (mode 'lex)]
    [("--parse")
      "Stop after parsing; print the parse tree"
      (mode 'parse)]
    [("--validate")
     "Stop after semantic analysis; print the validated tree"
     (mode 'validate)]
    [("--tacky")
      "Stop after TACKY lowering; print the IR"
      (mode 'tacky)]
    [("--codegen")
      "Stop after codegen; print the assembly tree"
      (mode 'codegen)]
    [("-S" "--assemble")
      "Stop after emit; write the .s file (skip linking)"
      (mode 'assemble)]
    #:once-any
    [("--target") t
     "Architecture to target (x86 or arm64)"
     (target (check-valid "--target" (string->symbol t) '(x86 arm64)))]
    #:args (filename)
    (if (string-suffix? filename ".c")
      (input-file filename)
      (raise-user-error
        'error
        "Invalid intput file extension. Expected a *.c file, but got ~a"
        filename))))


;; gcc : string? ... -> void?
;; Run gcc, wrapped with arch -x86_64 when targeting x86 on arm mac.
(define (gcc . args)
  (define gcc-path (find-executable-path "gcc"))
  (define (run . cmd)
    (match (apply system*/exit-code cmd)
      [0 (void)]
      [res (exit res)]))

  (case (target)
    ['x86 (apply run (find-executable-path "arch") "-x86_64" gcc-path args)]
    [else (apply run gcc-path args)]))


;; main : -> void?
;; Preprocess, compile, and optionally link the input C file.
(define (main)
  (let* ([executable-file (substring (input-file) 0 (- (string-length (input-file)) 2))]
         [preprocessed-file (string-append executable-file ".i")]
         [assembly-file (string-append executable-file ".s")])
    (gcc "-E" "-P" (input-file) "-o" preprocessed-file)
    (rcc-compile preprocessed-file (mode) (target) assembly-file)
    (delete-file preprocessed-file)
    (when (eq? (mode) 'full)
      (gcc assembly-file "-o" executable-file)
      (delete-file assembly-file))))

(module+ main
  (parse-args)
  (main))
