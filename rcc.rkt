#!/usr/bin/env racket
#lang racket

(require "compile.rkt")

;; mode : (parameter/c symbol?)
;; The compiler stage to stop at; defaults to a full compile.
(define mode (make-parameter 'full))
;; input-file : (parameter/c string?)
;; Path to the C source file to compile.
(define input-file (make-parameter "programs/return_2.c"))

;; parse-args : -> void?
;; Parse command-line flags and set the mode and input-file parameters.
(define (parse-args)
  (command-line
    #:program "rcc"
    #:once-any
    [("--lex")
      "Convert to tokens and print"
      (mode 'lex)]
    [("--parse")
      "Convert to parse tree and print"
      (mode 'parse)]
    [("--validate")
     "Run semantic analysis and print parse tree"
     (mode 'validate)]
    [("--tacky")
      "Convert to TACKY IR and print"
      (mode 'tacky)]
    [("--codegen")
      "Convert to assembly tree and print"
      (mode 'codegen)]
    [("-S" "--assemble")
      "Run the full compiler write an assembly file"
      (mode 'assemble)]
    #:args (filename)
    (if (string-suffix? filename ".c")
      (input-file filename)
      (raise-user-error
        'error
        "Invalid intput file extension. Expected a *.c file, but got ~a"
        filename))))


;; run : path-string? string? ... -> void?
;; Run an external command; exit with its code on failure.
(define (run . cmd)
  (match (apply system*/exit-code cmd)
    [0 (void)]
    [res (exit res)]))


;; main : -> void?
;; Preprocess, compile, and optionally link the input C file.
(define (main)
  (let* ([executable-file (substring (input-file) 0 (- (string-length (input-file)) 2))]
         [preprocessed-file (string-append executable-file ".i")]
         [assembly-file (string-append executable-file ".s")]
         [gcc-path (find-executable-path "gcc")])
    (run gcc-path "-E" "-P" (input-file) "-o" preprocessed-file)
    (rcc-compile preprocessed-file (mode) assembly-file)
    (delete-file preprocessed-file)
    (when (eq? (mode) 'full)
      (run gcc-path assembly-file "-o" executable-file)
      (delete-file assembly-file))))

(module+ main
  (parse-args)
  (main))
