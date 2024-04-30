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
                [(#f #f n) (format "~a:~a" name n)]
                [(line col _) (format "~a:~a:~a" name line col)]))

(define (format-loc span)
  (match span
    [(source-span name line col _ _) (format "~a:~a:~a" name line col)]))


(define (concat-srclocs loc-start loc-end)
  (match-let ([(source-span path start-line start-col _ _) loc-start]
              [(source-span _ _ _ end-line end-col) loc-end])
    (source-span path start-line start-col end-line end-col)))


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


(define (walk-tree fn tree)
  ; (debug 'walk-tree tree)
  (match tree
    [(? list?) (map fn tree)]

    [(program def loc) (program (fn def) loc)]

    ; functions can contain either a single statement or a list of instructions
    [(function name (list body-parts ...) loc) (function name (map fn body-parts) loc)]
    [(function name body loc) (function name (fn body) loc)]

    [(statement `(return ,expr) loc) (statement `(return ,(fn expr)) loc)]

    [(expr `(negate ,sub-expr) loc) (expr `(negate ,(fn sub-expr)) loc)]
    [(expr `(complement ,sub-expr) loc) (expr `(complement ,(fn sub-expr)) loc)]
    [(expr `(int ,_) _) tree]

    [(instruction `(negate ,op ,dest) loc) (instruction `(negate ,(fn op) ,(fn dest)) loc)]
    [(instruction `(complement ,op ,dest) loc) (instruction `(complement ,(fn op) ,(fn dest)) loc)]
    [(instruction `(return ,val) loc) (instruction `(return ,(fn val)) loc)]

    [(instruction `(mov ,what ,where) loc) (instruction `(mov ,(fn what) ,(fn where)) loc)]
    [(instruction `(neg ,what) loc) (instruction `(neg ,(fn what)) loc)]
    [(instruction `(not ,what) loc) (instruction `(not ,(fn what)) loc)]
    [(instruction `(allocate-stack ,_) loc) tree]
    [(instruction `(ret) _) tree]

    [(operand _ _) tree]))


(define ((top-down fn) tree)
  (walk-tree (top-down fn) (fn tree)))


(define ((bottom-up fn) tree)
  (fn (walk-tree (bottom-up fn) tree)))


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

        [(regexp-try-match #px"^[a-zA-Z_]\\w*\\b" in) => (decode-match 'ident start)]

        [(regexp-try-match #px"^[0-9]+\\b" in) => (decode-match 'const start)]

        [(regexp-try-match #px"^--" in) => (decode-match 'decrement start)]

        [(regexp-try-match #px"^-" in) => (decode-match 'negate start)]

        [(regexp-try-match #px"^~" in) => (decode-match 'complement start)]

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

  (define (replace-keywords tokens)
    (map
     (match-lambda
       [(token 'ident v loc)
        #:when (member v '("int" "void" "return"))
        (token 'keyword (string->symbol v) loc)]
       [t t])
     tokens))

  (replace-keywords (go)))


(define (parse tokens)
  (define (next-token-loc-str tokens)
    (match tokens
      ['() "EOF"]
      [(cons (token _ _ loc) _) (format-loc loc)]))

  (define (next-token-value tokens)
    (match tokens
      ['() "EOF"]
      [(cons (token _ value _) _) value]))

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

  (define ((peek-alternative name options) tokens)
    (define (peek? pats tokens)
      (match (cons pats tokens)
        [(cons '() _) #t]
        [(cons _ '()) #f]
        [(cons (cons (? symbol? pat) pats) (cons (token kind _ _) tokens))
         (and (equal? pat kind) (peek? pats tokens))]
        [(cons (cons (cons kind value) pats) (cons t tokens))
         (and (equal? kind (token-kind t)) (equal? value (token-value t)) (peek? pats tokens))]))
    (match (force options) ; options must be lazy to allow self reference
      ['() (raise-user-error 'error
                             "~a parser: Expected ~a but found ~a"
                             (next-token-loc-str tokens)
                             name
                             (next-token-value tokens))]
      [(cons (cons pats parser) rst)
       (if (peek? pats tokens)
           (parser tokens)
           ((peek-alternative name rst) tokens))]))

  (define parse-expr
    (peek-alternative "expression"
                      (delay
                        (list
                         (cons '(const)
                               (map/p (match-lambda [(token _ value loc)
                                                     (expr `(int ,(string->number value))
                                                           loc)])
                                      (expect-kind 'const)))
                         (cons '(negate)
                               (map/p (match-lambda [(list t e)
                                                     (expr `(negate ,e)
                                                           (concat-srclocs (token-loc t) (expr-loc e)))])
                                      (parse-sequence (expect-kind 'negate) parse-expr)))
                         (cons '(complement)
                               (map/p (match-lambda [(list t e)
                                                     (expr `(complement ,e)
                                                           (concat-srclocs (token-loc t) (expr-loc e)))])
                                      (parse-sequence (expect-kind 'complement) parse-expr)))
                         (cons '(lparen)
                               (map/p (match-lambda [(list _ e _) e])
                                      (parse-sequence (expect-kind 'lparen) parse-expr (expect-kind 'rparen))))))))

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


(define next-tacky-var 0)
(define (fresh-tacky-tmp-var loc)
  (let ([name (format "tmp.~a" next-tacky-var)])
    (set! next-tacky-var (add1 next-tacky-var))
    (operand `(var ,name) loc)))


(define gen-tacky
  (local [(define (unary? expr-kind)
            (member expr-kind '(negate complement)))]
    (bottom-up (match-lambda
                 [(expr `(int ,n) loc) (list (operand `(imm ,n) loc))]
                 [(expr `(,(? unary? kind) (,op ,@instructions)) loc)
                  (let* ([dest (fresh-tacky-tmp-var loc)]
                         [instr (instruction (list kind op dest) loc)])
                    (cons dest (cons instr instructions)))]
                 [(statement `(return (,(? operand? v) ,@instructions)) loc)
                  (reverse (cons (instruction `(return ,v) loc) instructions))]
                 [x x]))))


(define (assemble tacky)
  (define (unary? kind) (member kind '(negate complement)))

  (define convert-instruction-kind
    (match-lambda
      ['complement 'not]
      ['negate 'neg]))

  (define flatten-instruction-list
    (match-lambda
      ['() '()]
      [(cons (and fst `(,(? symbol?) ,@_)) rst) (cons fst (flatten-instruction-list rst))]
      [(cons (? instruction? fst) rst) (cons fst (flatten-instruction-list rst))]
      [(cons fst rst) (append (flatten-instruction-list fst) (flatten-instruction-list rst))]))

  (define rewrite-operators
    (bottom-up (match-lambda
                 [(instruction `(return ,op) loc)
                  (list (instruction `(mov ,op ,(operand `(reg AX) loc)) loc)
                        (instruction `(ret) loc))]
                 [(instruction `(,(? unary? kind) ,src ,dst) loc)
                  (list (instruction `(mov ,src ,dst) loc)
                        (instruction `(,(convert-instruction-kind kind) ,dst) loc))]
                 [(function name is loc) (function name (flatten-instruction-list is) loc)]
                 [x x])))

  (define stack-offset 0)
  (define (next-stack-offset)
    (set! stack-offset (+ 4 stack-offset))
    stack-offset)
  (define var-map (make-hash))
  (define replace-vars
    (bottom-up (match-lambda
                 [(operand `(var ,name) loc)
                  (let ([offset (hash-ref! var-map name next-stack-offset)])
                    (operand `(stack ,offset) loc))]
                 [(function name is loc)
                  (let ([res (function name (cons (instruction `(allocate-stack ,stack-offset) loc) is) loc)])
                    (set! stack-offset 0)
                    res)]
                 [x x])))

  (define (stack? op) (equal? 'stack (car (operand-value op))))
  (define fix-invalid-movs
    (bottom-up (match-lambda
                 [(instruction `(mov ,(? stack? src) ,(? stack? dst)) loc)
                  (let ([tmp-reg (operand `(reg R10) loc)])
                    (list (instruction `(mov ,src ,tmp-reg) loc)
                          (instruction `(mov ,tmp-reg ,dst) loc)))]
                 [(function name is loc) (function name (flatten-instruction-list is) loc)]
                 [x x])))

  (fix-invalid-movs (replace-vars (rewrite-operators tacky))))


(define (emit-assembly ast output-file)
  (define (operand->string o)
    (match o
      [(operand `(reg AX) _) "%eax"]
      [(operand `(reg R10) _) "%r10d"]
      [(operand `(stack ,n) _) (format "-~a(%rbp)" n)]
      [(operand `(imm ,value) _) (format "$~a" value)]))

  (define (ikind->string s)
    (match s
      ['mov "movl"]
      ['neg "negl"]
      ['not "notl"]))

  (define (emit-binop op a b)
    (printf "    ~a ~a, ~a\n" op a b))

  (define (emit-unop op a)
    (printf "    ~a ~a\n" op a))

  (define (emit-instruction i)
    (match i
      [(instruction `(allocate-stack ,n) _)
       (emit-binop "subq" (format "$~a" n) "%rsp")]
      [(instruction `(ret) _)
       (emit-binop "movq" "%rbp" "%rsp")
       (emit-unop "popq" "%rbp")
       (display "    ret\n")]
      [(instruction `(,op ,src ,dst) _)
       (emit-binop (ikind->string op) (operand->string src) (operand->string dst))]
      [(instruction `(,op ,value) _)
       (emit-unop (ikind->string op) (operand->string value))]))

  (define (emit-function fn)
    (match-let ([(function src-name body _) fn])
      (let ([name (string-append "_" src-name)])
        (printf "    .globl ~a\n~a:\n" name name)
        (emit-unop "pushq" "%rbp")
        (emit-binop "movq" "%rsp" "%rbp")
        (for ([i body])
          (emit-instruction i)))))

  (define (emit-program ast)
    (match-let ([(program fn _) ast])
      (emit-function fn)))

  (with-output-to-file output-file (λ () (emit-program ast)) #:exists 'replace))


(module+ main
  (rcc-compile "programs/return_2.c" 'assemble "programs/return_2.s"))
