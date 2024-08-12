#lang racket

(require "utils.rkt")
(provide lex)


(define (replace-keywords tokens)
  (map
   (match-lambda
     [`(ident ,v ,loc)
      #:when (member? v '("int" "void" "return"))
      `(keyword ,(string->symbol v) ,loc)]
     [t t])
   tokens))


(define (lex input-file-name)
  (define in (open-input-file input-file-name))
  (port-count-lines! in)

  (define ((decode-match symb start) data)
    (let*-values ([(value) (bytes->string/utf-8 (car data))]
                  [(row col _) (port-next-location in)]
                  [(loc) (span input-file-name (car start) (cdr start) row col)]
                  [(decoded) (list symb value loc)])
      (cons decoded (go))))

  (define (go)
    (let*-values ([(row col _) (port-next-location in)]
                  [(start) (cons row col)])
      (cond
        [(regexp-try-match #px"^\\s+" in) (go)]

        [(regexp-try-match #px"^[a-zA-Z_]\\w*\\b" in) => (decode-match 'ident start)]

        [(regexp-try-match #px"^[0-9]+\\b" in) => (decode-match 'const start)]

        [(regexp-try-match #px"^--" in) => (decode-match 'decrement start)]

        ; arithmetic operators
        [(regexp-try-match #px"^-" in)   => (decode-match 'negate start)]
        [(regexp-try-match #px"^\\+" in) => (decode-match 'add start)]
        [(regexp-try-match #px"^\\*" in) => (decode-match 'multiply start)]
        [(regexp-try-match #px"^/" in)   => (decode-match 'divide start)]
        [(regexp-try-match #px"^%" in)   => (decode-match 'remainder start)]

        ; shift operators
        [(regexp-try-match #px"^>>" in)  => (decode-match 'rshift start)]
        [(regexp-try-match #px"^<<" in)  => (decode-match 'lshift start)]

        ; logical operators
        [(regexp-try-match #px"^==" in)     => (decode-match 'equal start)]
        [(regexp-try-match #px"^!=" in)     => (decode-match 'not-equal start)]
        [(regexp-try-match #px"^!" in)      => (decode-match 'not start)]
        [(regexp-try-match #px"^&&" in)     => (decode-match 'and start)]
        [(regexp-try-match #px"^\\|\\|" in) => (decode-match 'or start)]
        [(regexp-try-match #px"^<=" in)     => (decode-match 'less-or-equal start)]
        [(regexp-try-match #px"^>=" in)     => (decode-match 'greater-or-equal start)]
        [(regexp-try-match #px"^<" in)      => (decode-match 'less-than start)]
        [(regexp-try-match #px"^>" in)      => (decode-match 'greater-than start)]

        ; bitwise operators
        [(regexp-try-match #px"^~" in)   => (decode-match 'complement start)]
        [(regexp-try-match #px"^&" in)   => (decode-match 'bitwise-and start)]
        [(regexp-try-match #px"^\\|" in) => (decode-match 'bitwise-or start)]
        [(regexp-try-match #px"^\\^" in) => (decode-match 'bitwise-xor start)]

        ; punctuation
        [(regexp-try-match #px"^\\(" in) => (decode-match 'lparen start)]
        [(regexp-try-match #px"^\\)" in) => (decode-match 'rparen start)]
        [(regexp-try-match #px"^\\{" in) => (decode-match 'lbrace start)]
        [(regexp-try-match #px"^\\}" in) => (decode-match 'rbrace start)]
        [(regexp-try-match #px"^\\;" in) => (decode-match 'semicolon start)]

        [(eq? (peek-char in) eof) '()]

        [else (raise-user-error
               'error
               "~a: lexer: Unrecognized token ~a"
               (format-srcloc input-file-name in)
               (peek-char in))])))

  (replace-keywords (go)))
