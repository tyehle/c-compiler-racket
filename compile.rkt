#lang racket

(provide rcc-compile)


(struct span (path start-line start-col stop-line stop-col))


(define (debug label v)
  (printf "~a" label)
  (pretty-print v)
  v)


(define (format-srcloc name file)
  (match/values (port-next-location file)
                [(#f #f n) (format "~a:~a" name n)]
                [(line col _) (format "~a:~a:~a" name line col)]))

(define format-loc (match-lambda
                     [(span name line col _ _) (format "~a:~a:~a" name line col)]))


(define (concat-srclocs loc-start loc-end)
  (match-let ([(span path start-line start-col _ _) loc-start]
              [(span _ _ _ end-line end-col) loc-end])
    (span path start-line start-col end-line end-col)))


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
  (define splice-nodes (match-lambda
                         ['() '()]
                         [(cons (and fst (cons (? symbol?) _)) rst) (cons fst (splice-nodes rst))]
                         [(cons fst rst) (append fst (splice-nodes rst))]))

  ; (debug 'walk-tree tree)
  (match tree
    [`(,(? symbol? kind) ,@vs ,(? span? loc)) `(,kind ,@(map fn vs) ,loc)]
    [`(,(? symbol? kind) ,@vs) `(,kind ,@(map fn vs))]
    [(? list?) (splice-nodes (map fn tree))]
    [x x]))


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
                  [(loc) (span input-file-name (car start) (cdr start) row col)]
                  [(decoded) (list symb value loc)])
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
       [`(ident ,v ,loc)
        #:when (member v '("int" "void" "return"))
        `(keyword ,(string->symbol v) ,loc)]
       [t t])
     tokens))

  (replace-keywords (go)))


(define (parse tokens)
  (define (next-token-loc-str tokens)
    (match tokens
      ['() "EOF"]
      [`((,_ ,_ ,loc) ,@_) (format-loc loc)]))

  (define (next-token-value tokens)
    (match tokens
      ['() "EOF"]
      [`((,_ ,value ,_) ,@_) value]))

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
      [`((,k ,_ ,_) ,@rst) #:when (equal? k kind) tokens]
      [`((,actual-kind ,_ ,loc) ,@_)
       (raise-user-error
        'error
        "~a: parser: Expected ~a but found ~a"
        (format-loc loc)
        kind
        actual-kind)]))

  (define  ((expect kind value) tokens)
    (match ((expect-kind kind) tokens)
      [`((,_ ,v ,_) ,@_) #:when (equal? v value) tokens]
      [`((,_ ,actual-value ,loc) ,@_)
       (raise-user-error
        'error
        "~a parser: Expected ~a ~a, but found ~a"
        (format-loc loc)
        kind
        value
        actual-value)]))

  (define ((peek-alternative name options) tokens)
    (define (peek? pats tokens)
      (match (cons pats tokens)
        [(cons '() _) #t]
        [(cons _ '()) #f]
        [`((,(? symbol? pat) ,@pats) . ((,kind ,_ ,_) ,@tokens))
         (and (equal? pat kind) (peek? pats tokens))]
        [`(((,kind . ,value) ,@pats) . ((,k ,v ,_) ,@tokens))
         (and (equal? kind k) (equal? value v) (peek? pats tokens))]))
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
                               (map/p (match-lambda [`(const ,value ,loc)
                                                     `(int ,(string->number value) ,loc)])
                                      (expect-kind 'const)))
                         (cons '(negate)
                               (map/p (match-lambda [`((negate ,_ ,t-loc) ,(and e `(,@_ ,e-loc)))
                                                     (let ([loc (concat-srclocs t-loc e-loc)])
                                                       `(negate ,e ,loc))])
                                      (parse-sequence (expect-kind 'negate) parse-expr)))
                         (cons '(complement)
                               (map/p (match-lambda [`((complement ,_ ,t-loc) ,(and e `(,@_ ,e-loc)))
                                                     (let ([loc (concat-srclocs t-loc e-loc)])
                                                       `(complement ,e ,loc))])
                                      (parse-sequence (expect-kind 'complement) parse-expr)))
                         (cons '(lparen)
                               (map/p (match-lambda [(list _ e _) e])
                                      (parse-sequence (expect-kind 'lparen)
                                                      parse-expr
                                                      (expect-kind 'rparen))))))))

  (define parse-statement
    (map/p
     (match-lambda
       [`((,_ ,_ ,loc-start) ,expr (,_ ,_ ,loc-end))
        `(return ,expr ,(concat-srclocs loc-start loc-end))])
     (parse-sequence (expect 'keyword 'return) parse-expr (expect-kind 'semicolon))))

  (define parse-function
    (map/p
     (match-lambda
       [`((,_ ,_ ,loc-start) (ident ,name ,_) ,@_..4 ,body (,_ ,_ ,loc-end))
        `(function ,name ,body ,(concat-srclocs loc-start loc-end))])
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
     (match-lambda [(and f `(function ,_ ,_ ,loc)) `(program ,f ,loc)])
     parse-function))

  (match (parse-program tokens)
    [(cons prog '()) prog]
    [(cons _ `((,kind ,value ,loc) ,@_))
     (raise-user-error 'error "~a: paser: Expected eof, but found ~a ~a" (format-loc loc) kind value)]))


(define next-tacky-var 0)
(define (fresh-tacky-tmp-var loc)
  (let ([name (format "tmp.~a" next-tacky-var)])
    (set! next-tacky-var (add1 next-tacky-var))
    `(var ,name ,loc)))


(define gen-tacky
  (local [(define (unary? expr-kind)
            (member expr-kind '(negate complement)))]
    (bottom-up (match-lambda
                 [`(int ,n ,loc) (list `(imm ,n ,loc))]
                 [`(,(? unary? kind) (,op ,@instructions) ,loc)
                  (let* ([dest (fresh-tacky-tmp-var loc)]
                         [instr `(,kind ,op ,dest ,loc)])
                    (cons dest (cons instr instructions)))]
                 [`(return (,v ,@instructions) ,loc)
                  (reverse (cons `(return ,v ,loc) instructions))]
                 [x x]))))


(define (assemble tacky)
  (define (unary? kind) (member kind '(negate complement)))

  (define convert-instruction-kind
    (match-lambda
      ['complement 'not]
      ['negate 'neg]))

  (define rewrite-operators
    (bottom-up (match-lambda
                 [`(return ,op ,loc)
                  (list `(mov ,op (reg AX ,loc) ,loc)
                        `(ret, loc))]
                 [`(,(? unary? kind) ,src ,dst ,loc)
                  (list `(mov ,src ,dst ,loc)
                        `(,(convert-instruction-kind kind) ,dst ,loc))]
                 [x x])))

  (define stack-offset 0)
  (define (next-stack-offset)
    (set! stack-offset (+ 4 stack-offset))
    stack-offset)
  (define var-map (make-hash))
  (define replace-vars
    (bottom-up (match-lambda
                 [`(var ,name ,loc)
                  (let ([offset (hash-ref! var-map name next-stack-offset)])
                    `(stack ,offset ,loc))]
                 [`(function ,name ,is ,loc)
                  (let ([final-offset stack-offset])
                    (set! stack-offset 0)
                    `(function ,name ((allocate-stack ,final-offset ,loc) ,@is) ,loc))]
                 [x x])))

  (define (stack? op) (equal? 'stack (car op)))
  (define fix-invalid-movs
    (bottom-up (match-lambda
                 [`(mov ,(? stack? src) ,(? stack? dst) ,loc)
                  (let ([tmp-reg `(reg R10 ,loc)])
                    (list `(mov ,src ,tmp-reg ,loc)
                          `(mov ,tmp-reg ,dst ,loc)))]
                 [x x])))

  (fix-invalid-movs (replace-vars (rewrite-operators tacky))))


(define (emit-assembly ast output-file)
  (define (operand->string o)
    (match o
      [`(reg AX ,_) "%eax"]
      [`(reg R10 ,_) "%r10d"]
      [`(stack ,n ,_) (format "-~a(%rbp)" n)]
      [`(imm ,value ,_) (format "$~a" value)]))

  (define instruction-map
    (hash 'mov "movl"
          'neg "negl"
          'not "notl"))
  (define (instruction? i) (dict-has-key? instruction-map i))
  (define (instruction->string i) (dict-ref instruction-map i))

  (define (emit-binop op a b)
    (printf "    ~a ~a, ~a\n" op a b))

  (define (emit-unop op a)
    (printf "    ~a ~a\n" op a))

  (define (emit ast)
    (match ast
      [`(program ,fn ,_) (emit fn)]

      [`(function ,src-name ,body ,_)
       (let ([name (string-append "_" src-name)])
         (printf "    .globl ~a\n~a:\n" name name)
         (emit-unop "pushq" "%rbp")
         (emit-binop "movq" "%rsp" "%rbp")
         (for ([i body])
           (emit i)))]

      [`(allocate-stack ,n ,_)
       (emit-binop "subq" (format "$~a" n) "%rsp")]
      [`(ret ,_)
       (emit-binop "movq" "%rbp" "%rsp")
       (emit-unop "popq" "%rbp")
       (display "    ret\n")]
      [`(,(? instruction? op) ,src ,dst ,_)
       (emit-binop (instruction->string op) (operand->string src) (operand->string dst))]
      [`(,(? instruction? op) ,value ,_)
       (emit-unop (instruction->string op) (operand->string value))]))

  (with-output-to-file output-file (λ () (emit ast)) #:exists 'replace))


(module+ main
  (rcc-compile "programs/return_2.c" 'tacky "programs/return_2.s"))
