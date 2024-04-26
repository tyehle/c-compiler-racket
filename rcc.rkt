#!/usr/bin/env racket
#lang racket

(require "compile.rkt")

(define mode (make-parameter 'full))
(define input-file (make-parameter "programs/return_2.c"))

(define (parse-args)
  (command-line
    #:program "rcc"
    #:once-any
    [("--lex")
      "Run the lexer and stop before parsing"
      (mode 'lex)]
    [("--parse")
      "Run the lexer and the parser and stop before assembly generation"
      (mode 'parse)]
    [("--codegen")
      "Run the lexer, parser, and assembler and stop before code emmision"
      (mode 'codegen)]
    [("-S" "--assemble")
      "Run the full compiler and stop before linking"
      (mode 'assemble)]
    #:args (filename)
    (if (string-suffix? filename ".c")
      (input-file filename)
      (raise-user-error
        'error
        "Invalid intput file extension. Expected a *.c file, but got ~a"
        filename))))


(define (run . cmd)
  (match (apply system*/exit-code cmd)
    [0 (void)]
    [res (exit res)]))


(define (main)
  (let* ([executable-file (substring (input-file) 0 (- (string-length (input-file)) 2))]
         [preprocessed-file (string-append executable-file ".i")]
         [assembly-file (string-append executable-file ".s")]
         [gcc-path (find-executable-path "gcc")])
    (run gcc-path "-E" "-P" (input-file) "-o" preprocessed-file)
    (rcc-compile preprocessed-file (mode) assembly-file)
    (delete-file preprocessed-file)
    (if (eq? (mode) 'full)
      (begin
        (run gcc-path assembly-file "-o" executable-file)
        (delete-file assembly-file))
      (void))))

(module+ main
  (parse-args)
  (main))
