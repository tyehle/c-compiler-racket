#lang racket

(require "utils.rkt" "schema.rkt")
(provide lex)


;; ensure-schema : any/c -> any/c
;; Validate that value is a well-formed token list.
;; Returns value unchanged, or raises an error if validation fails.
(define (ensure-schema value)
  ;; Recognize non-keyword token kinds.
  (define named
    (schema-any 'lparen 'rparen 'lbrace 'rbrace 'semicolon
                'ident 'const
                'assign
                'complement 'not
                'multiply 'divide 'remainder
                'add 'negate
                'lshift 'rshift
                'less-than 'less-or-equal 'greater-than 'greater-or-equal 'equal 'not-equal
                'bitwise-and 'bitwise-xor 'bitwise-or
                'and 'or))
  ;; Recognize keyword token values.
  (define keywd
    (schema-any 'return 'void 'int))
  ;; Schema for a single token: either a keyword or a named token.
  (define token
    (schema-any
      (list 'keyword keywd span?)
      (list named string? span?)))
  (check-schema value (schema-many token) (schema-error-proc "Invalid token stream" value))
  value)


;; format-srcloc : string? input-port? -> string?
;; Format the current position in file as "name:line:col" or "name:offset".
(define (format-srcloc name file)
  (match/values (port-next-location file)
                [(#f #f n) (format "~a:~a" name n)]
                [(line col _) (format "~a:~a:~a" name line col)]))


;; replace-keywords : (listof token?) -> (listof token?)
;; Convert ident tokens whose value is a reserved word into keyword tokens.
(define (replace-keywords tokens)
  (map
   (match-lambda
     [`(ident ,v ,loc)
      #:when (member? v '("int" "void" "return"))
      `(keyword ,(string->symbol v) ,loc)]
     [t t])
   tokens))


;; lex : string? -> (listof token?)
;; Read the file at input-file-name and return a flat list of tokens,
;; with keywords distinguished from plain identifiers.
(define (lex input-file-name)
  (define in (open-input-file input-file-name))
  (port-count-lines! in)

  ;; decode-match : symbol? (cons integer? integer?) -> (bytes? -> (listof token?))
  ;; Build a handler for regexp-try-match: decode the matched bytes into a
  ;; token with the given kind and source span, then continue lexing.
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

        ; assignment
        [(regexp-try-match #px"^=" in) => (decode-match 'assign start)]

        [(eq? (peek-char in) eof) '()]

        [else (raise-user-error
               'error
               "~a: lexer: Unrecognized token ~a"
               (format-srcloc input-file-name in)
               (peek-char in))])))

  (ensure-schema (replace-keywords (go))))
