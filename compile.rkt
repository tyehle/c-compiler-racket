#lang racket

(provide rcc-compile)


(struct source-span (path start-line start-col stop-line stop-col))

(struct token (kind value loc) #:transparent)

(struct program (definition loc) #:transparent)
(struct function (name body loc) #:transparent)
(struct statement (value loc) #:transparent)
(struct expr (value loc) #:transparent)

(struct instruction (value loc) #:transparent)
(struct operand (value loc) #:transparent)


(define (debug symb v)
  (printf "~v: " symb)
  (pretty-print v)
  v)


(define (format-srcloc name file)
  (match/values (port-next-location file)
                [(#f #f n) (format "file ~a, byte ~a" name n)]
                [(line col _) (format "file ~a, line ~a, col ~a" name line col)]))

(define (format-loc span)
  (match span
    [(source-span name line col _ _) (format "file ~a, line ~a, col ~a" name line col)]))


(define (concat-srclocs loc-start loc-end)
  (match-let ([(source-span path start-line start-col _ _) loc-start]
              [(source-span _ _ _ end-line end-col) loc-end])
    (source-span path start-line start-col end-line end-col)))


(define (rcc-compile input-file mode assembly-file)
  (let* ([tokens (delay (lex input-file))]
         [ast (delay (parse (force tokens)))]
         [assembly (delay (assemble (force ast)))])
    (match mode
      ['lex (pretty-print (force tokens))]
      ['parse (pretty-print (force ast))]
      ['codegen (pretty-print (force assembly))]
      [(or 'assemble 'full) (emit-assembly (force assembly) assembly-file)])))


(define (lex input-file-name)
  (define in (open-input-file input-file-name))
  (port-count-lines! in)

  (define ((decode-match symb start) data)
    (let*-values ([(value) (bytes->string/utf-8 (car data))]
                  [(row col _) (port-next-location in)]
                  [(loc) (source-span input-file-name (car start) (cdr start) row col)]
                  [(decoded) (token symb value loc)])
      ; (println decoded)
      (cons decoded (go))))

  (define (go)
    (let*-values ([(row col _) (port-next-location in)]
                  [(start) (cons row col)])
      (cond
        [(regexp-try-match #px"^\\s+" in) (go)]

        [(regexp-try-match #px"^[a-zA-Z_]\\w*\\b" in)
         => (decode-match 'ident start)]

        [(regexp-try-match #px"^[0-9]+\\b" in)
         => (decode-match 'const start)]

        [(regexp-try-match #px"^\\(" in)
         => (decode-match 'lparen start)]

        [(regexp-try-match #px"^\\)" in)
         => (decode-match 'rparen start)]

        [(regexp-try-match #px"^\\{" in)
         => (decode-match 'lbrace start)]

        [(regexp-try-match #px"^\\}" in)
         => (decode-match 'rbrace start)]

        [(regexp-try-match #px"^\\;" in)
         => (decode-match 'semicolon start)]

        [(eq? (peek-char in) eof) '()]

        [else (raise-user-error
               'error
               "~a: lexer: Unrecognized token ~a"
               (format-srcloc input-file-name in)
               (peek-char in))])))

  (define (replace-keywords tokens)
    (map
     (match-lambda
       [(token 'ident v loc)
        #:when (member v '("int" "void" "return"))
        (token 'keyword (string->symbol v) loc)]
       [t t])
     tokens))

  (define (replace-ints tokens)
    (map
     (match-lambda
       [(token 'const str loc) (token 'const (string->number str) loc)]
       [t t])
     tokens))

  (replace-ints (replace-keywords (go))))


(define (parse tokens)
  (define ((parse-sequence . parsers) tokens)
    (match parsers
      ['() (cons '() tokens)]
      [(cons parser rst)
       (match-let* ([(cons result remaining-tokens) (parser tokens)]
                    [(cons seq-results final-tokens) ((apply parse-sequence rst) remaining-tokens)])
         (cons (cons result seq-results) final-tokens))]))

  (define ((map/p fn parser) tokens)
    (match (parser tokens)
      [(cons x tokens) (cons (fn x) tokens)]))

  (define ((expect-kind kind) tokens)
    (match tokens
      ['() (raise-user-error 'error "parser: Expected ~a, but reached end of input" kind)]
      [(cons (and t (token (? (λ (v) (equal? v kind))) _ _)) rst) (cons t rst)]
      [(cons (token actual-kind _ where) _)
       (raise-user-error
        'error
        "~a: parser: Expected ~a but found ~a"
        (format-loc where)
        kind
        actual-kind)]))

  (define  ((expect kind value) tokens)
    (match ((expect-kind kind) tokens)
      [(cons (and t (token _ (? (λ (v) (equal? v value))) _)) rst) (cons t rst)]
      [(cons (token _ actual-value where) _)
       (raise-user-error
        'error
        "~a parser: Expected ~a ~a, but found ~a"
        (format-loc where)
        kind
        value
        actual-value)]))

  (define parse-expr
    (map/p
     (λ (t) (expr `(int ,(token-value t)) (token-loc t)))
     (expect-kind 'const)))

  (define parse-statement
    (map/p
     (match-lambda
       [(list (token _ _ loc-start) expr (token _ _ loc-end))
        (statement `(return ,expr) (concat-srclocs loc-start loc-end))])
     (parse-sequence (expect 'keyword 'return) parse-expr (expect-kind 'semicolon))))

  (define parse-function
    (map/p
     (match-lambda
       [(list (token _ _ loc-start) (token 'ident name _) _ _ _ _ body (token _ _ loc-end))
        (function name body (concat-srclocs loc-start loc-end))])
     (parse-sequence
      (expect 'keyword 'int)
      (expect-kind 'ident)
      (expect-kind 'lparen)
      (expect 'keyword 'void)
      (expect-kind 'rparen)
      (expect-kind 'lbrace)
      parse-statement
      (expect-kind 'rbrace))))

  (define parse-program
    (map/p
     (λ (function) (program function (function-loc function)))
     parse-function))

  (match (parse-program tokens)
    [(cons prog '()) prog]
    [(cons _ (cons (token kind value loc) _))
     (raise-user-error 'error "~a: paser: Expected eof, but found ~a ~a" (format-loc loc) kind value)]))


(define (assemble ast)
  (define (assemble-expr e)
    (match e
      [(expr `(int ,value) loc) (operand `(imm ,value) loc)]))

  (define (assemble-statement s)
    (match s
      [(statement `(return ,expr) loc)
       (list (instruction `(mov ,(assemble-expr expr) ,(operand `(reg eax) loc)) loc)
             (instruction '(ret) loc))]))

  (define (assemble-function fn)
    (match-let ([(function name body loc) fn])
      (function name (assemble-statement body) loc)))

  (define (assemble-program p)
    (match-let ([(program fn loc) p])
      (program (assemble-function fn) loc)))

  (assemble-program ast))


(define (emit-assembly ast output-file)
  (define (emit-operand o)
    (match o
      [(operand `(reg ,name) _) (printf "%~a" name)]
      [(operand `(imm ,value) _) (printf "$~a" value)]))

  (define (emit-instruction i)
    (match i
      [(instruction `(mov ,what ,where) _)
       (display "    movl ")
       (emit-operand what)
       (display ", ")
       (emit-operand where)
       (display "\n")]
      [(instruction `(ret) _)
       (display "    ret\n")]))

  (define (emit-function fn)
    (match-let ([(function src-name body _) fn])
      (let ([name (string-append "_" src-name)])
        (printf "    .globl ~a\n~a:\n" name name)
        (for ([i body])
          (emit-instruction i)))))

  (define (emit-program ast)
    (match-let ([(program fn _) ast])
      (emit-function fn)))

  (with-output-to-file output-file (λ () (emit-program ast)) #:exists 'replace))


(module+ main
  (rcc-compile "programs/return_2.c" 'assemble "programs/return_2.s"))
