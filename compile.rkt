#lang racket

(provide rcc-compile)


(struct span (path start-line start-col stop-line stop-col))


(define (debug label v)
  (printf "\n~a\n" label)
  (pretty-print v)
  v)


(define (format-srcloc name file)
  (match/values (port-next-location file)
                [(#f #f n) (format "~a:~a" name n)]
                [(line col _) (format "~a:~a:~a" name line col)]))


(define format-loc (match-lambda
                     [(span name line col _ _) (format "~a:~a:~a" name line col)]))


(define (join-locs a b)
  (match (cons a b)
    [`((,@_ ,(span path start-line start-col _ _)) . (,@_ ,(span _ _ _ end-line end-col)))
     (span path start-line start-col end-line end-col)]))


(define (member? x xs)
  (match xs
    ['() #f]
    [(cons fst rst) (or (equal? fst x) (member? x rst))]))


(define ((contains? . xs) x)
  (member? x xs))


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

  (define (replace-keywords tokens)
    (map
     (match-lambda
       [`(ident ,v ,loc)
        #:when (member? v '("int" "void" "return"))
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
      [`((,k ,_ ,_) ,@_) #:when (equal? k kind) tokens]
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

  (define (any-token tokens)
    (match tokens
      ['() (raise-user-error 'error "parser: Expected token, but reached end of input")]
      [_ tokens]))

  (define ((peek-alternative name options) tokens)
    (define (peek? pats tokens)
      (match (cons pats tokens)
        [(cons '() _) #t]
        [(cons _ '()) #f]
        [`((,(? symbol? pat) ,@pats) . ((,kind ,_ ,_) ,@tokens))
         (and (equal? pat kind) (peek? pats tokens))]
        [`(((,kind . ,value) ,@pats) . ((,k ,v ,_) ,@tokens))
         (and (equal? kind k) (equal? value v) (peek? pats tokens))]
        [`((,pat ,@pats) . (,t ,@tokens)) (and (pat t) (peek? pats tokens))]))
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

  (define ((is-kind? . ks) tok)
    (member? (first tok) ks))

  (define parse-factor
    (peek-alternative "expression"
                      (delay
                        (list
                         (cons (list (is-kind? 'const))
                               (map/p (match-lambda [`(const ,value ,loc)
                                                     `(int ,(string->number value) ,loc)])
                                      any-token))
                         (cons (list (is-kind? 'complement 'negate 'not))
                               (map/p (match-lambda [`(,(and tok `(,kind ,_ ,_)) ,e)
                                                     (let ([loc (join-locs tok e)])
                                                       `(,kind ,e ,loc))])
                                      (parse-sequence any-token parse-factor)))
                         (cons (list (is-kind? 'lparen))
                               (map/p (match-lambda [(list _ e _) e])
                                      (parse-sequence any-token
                                                      (parse-expr 0)
                                                      (expect-kind 'rparen))))))))

  (define ((parse-expr min-prec) t1)
    (define precedence (hash 'multiply         50
                             'divide           50
                             'remainder        50
                             'add              45
                             'negate           45
                             'lshift           40
                             'rshift           40
                             'less-than        35
                             'less-or-equal    35
                             'greater-than     35
                             'greater-or-equal 35
                             'equal            30
                             'not-equal        30
                             'bitwise-and      25
                             'bitwise-xor      20
                             'bitwise-or       15
                             'and              10
                             'or               5))

    (define (valid-operator? token)
      ((hash-ref precedence (first token) -1) . >= . min-prec))

    (define ((rec left) t1)
      (match t1
        [(cons (? valid-operator? op-token) t2)
         (match-let* ([`(,op ,_ ,_) op-token]
                      [op-prec (hash-ref precedence op)]
                      [(cons right t3) ((parse-expr (+ 1 op-prec)) t2)]
                      [new-left `(,op ,left ,right ,(join-locs left right))])
           ((rec new-left) t3))]
        [_ (cons left t1)]))

    (match-let ([(cons left t2) (parse-factor t1)])
      ((rec left) t2)))

  (define parse-statement
    (map/p
     (match-lambda
       [`(,start ,expr ,end)
        `(return ,expr ,(join-locs start end))])
     (parse-sequence (expect 'keyword 'return) (parse-expr 0) (expect-kind 'semicolon))))

  (define parse-function
    (map/p
     (match-lambda
       [`(,start (ident ,name ,_) ,@_..4 ,body ,end)
        `(function ,name ,body ,(join-locs start end))])
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

  (define rename-subtract
    (bottom-up (match-lambda
                 [`(negate ,a ,b ,loc) `(subtract ,a ,b ,loc)]
                 [x x])))

  (match (rename-subtract (parse-program tokens))
    [(cons prog '()) prog]
    [(cons _ `((,kind ,value ,loc) ,@_))
     (raise-user-error 'error "~a: paser: Expected eof, but found ~a ~a" (format-loc loc) kind value)]))


(define next-tacky-var 0)
(define (fresh-tacky-tmp-var loc)
  (let ([name (format "tmp.~a" next-tacky-var)])
    (set! next-tacky-var (add1 next-tacky-var))
    `(var ,name ,loc)))
(define (fresh-tacky-label hint)
  (let [(name (format "~a_~a" hint next-tacky-var))]
    (set! next-tacky-var (add1 next-tacky-var))
    name))
(define gen-tacky
  (let [(unary? (contains? 'negate 'complement 'not))
        (binary? (contains? 'add 'subtract 'multiply 'divide 'remainder
                            'equal 'not-equal
                            'less-than 'less-or-equal 'greater-than 'greater-or-equal
                            'lshift 'rshift
                            'bitwise-and 'bitwise-xor 'bitwise-or))]
    (bottom-up (match-lambda
                 [`(int ,n ,loc) (list `(imm ,n ,loc))]
                 [`(,(? unary? kind) (,operand ,@instructions) ,loc)
                  (let* ([dest (fresh-tacky-tmp-var loc)]
                         [instr `(,kind ,operand ,dest ,loc)])
                    (cons dest (cons instr instructions)))]
                 [`(return (,v ,@instructions) ,loc)
                  (reverse (cons `(return ,v ,loc) instructions))]
                 [`(,(? binary? kind) (,a ,@a-instrs) (,b ,@b-instrs) ,loc)
                  (let* ([dest (fresh-tacky-tmp-var loc)]
                         [instr `(,kind ,a ,b ,dest ,loc)])
                    (cons dest (cons instr (append a-instrs b-instrs))))]
                 [`(and (,fst-val ,@fst) (,snd-val ,@snd) ,loc)
                  (let [(result (fresh-tacky-tmp-var loc))
                        (false-label (fresh-tacky-label 'and_false))
                        (end-label (fresh-tacky-label 'and_end))]
                    `(,result
                      (label ,end-label ,loc)
                      (copy (imm 0 ,loc) ,result, loc)
                      (label ,false-label ,loc)
                      (jump ,end-label ,loc)
                      (copy (imm 1 ,loc) ,result ,loc)
                      (jump-if-zero ,snd-val ,false-label ,loc)
                      ,@snd
                      (jump-if-zero ,fst-val ,false-label ,loc)
                      ,@fst))]
                 [`(or (,fst-val ,@fst) (,snd-val ,@snd) ,loc)
                  (let [(result (fresh-tacky-tmp-var loc))
                        (true-label (fresh-tacky-label 'or_true))
                        (end-label (fresh-tacky-label 'or_end))]
                    `(,result
                      (label ,end-label ,loc)
                      (copy (imm 1 ,loc) ,result, loc)
                      (label ,true-label ,loc)
                      (jump ,end-label ,loc)
                      (copy (imm 0 ,loc) ,result ,loc)
                      (jump-if-not-zero ,snd-val ,true-label ,loc)
                      ,@snd
                      (jump-if-not-zero ,fst-val ,true-label ,loc)
                      ,@fst))]
                 [x x]))))


(define (assemble tacky)
  (define unary? (contains? 'negate 'complement))
  (define standard-binary? (contains? 'add 'subtract 'multiply 'lshift 'rshift 'bitwise-and 'bitwise-xor 'bitwise-or))
  (define cond-jump? (contains? 'jump-if-zero 'jump-if-not-zero))
  (define relational? (contains? 'equal 'not-equal
                                 'less-than 'less-or-equal
                                 'greater-than 'greater-or-equal))

  (define convert-instruction-kind
    (match-lambda
      ['complement 'not]
      ['negate 'neg]
      ['add 'add]
      ['subtract 'sub]
      ['multiply 'imul]
      ['divide 'idiv]
      ['lshift 'sal]
      ['rshift 'sar]
      ['bitwise-and 'bitwise-and]
      ['bitwise-xor 'bitwise-xor]
      ['bitwise-or 'bitwise-or]))

  (define rewrite-operators
    (bottom-up (match-lambda
                 [`(return ,op ,loc)
                  `((mov ,op (reg AX ,loc) ,loc)
                    (ret, loc))]
                 [`(divide ,a ,b ,dst ,loc)
                  `((mov ,a (reg AX ,loc) ,loc)
                    (cdq ,loc)
                    (idiv ,b ,loc)
                    (mov (reg AX ,loc) ,dst ,loc))]
                 [`(remainder ,a ,b ,dst ,loc)
                  `((mov ,a (reg AX ,loc) ,loc)
                    (cdq ,loc)
                    (idiv ,b ,loc)
                    (mov (reg DX ,loc) ,dst ,loc))]
                 [`(,(? cond-jump? kind) ,what ,where ,loc)
                  `((cmp (imm 0 ,loc) ,what ,loc)
                    (jmp-cc ,kind ,where ,loc))]
                 [`(,(? relational? kind) ,left ,right ,dst ,loc)
                  `((cmp ,right ,left ,loc)
                    (mov (imm 0 ,loc) ,dst ,loc)
                    (set-cc ,kind ,dst ,loc))]
                 [`(not ,src ,dst ,loc)
                  `((cmp (imm 0 ,loc) ,src ,loc)
                    (mov (imm 0 ,loc) ,dst ,loc)
                    (set-cc equal ,dst ,loc))]
                 [`(jump ,where ,loc)
                  `(jmp ,where ,loc)]
                 [`(copy ,src ,dst ,loc)
                  `(mov ,src ,dst ,loc)]
                 [`(,(? unary? kind) ,src ,dst ,loc)
                  `((mov ,src ,dst ,loc)
                    (,(convert-instruction-kind kind) ,dst ,loc))]
                 [`(,(? standard-binary? kind) ,a ,b ,dst ,loc)
                  `((mov ,a ,dst ,loc)
                    (,(convert-instruction-kind kind) ,b ,dst ,loc))]
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
  (define (imm? op) (equal? 'imm (car op)))
  (define fix-invalid-movs
    (bottom-up (match-lambda
                 ; idiv cannot operate on a constant
                 [`(idiv ,(and val `(imm ,_ ,_)) ,loc)
                  `((mov ,val (reg R10 ,loc) ,loc)
                    (idiv (reg R10 ,loc) ,loc))]
                 ; imul can't use an address as its destination
                 [`(imul ,src ,(? stack? dst) ,loc)
                  `((mov ,dst (reg R11 ,loc) ,loc)
                    (imul ,src (reg R11 ,loc) ,loc)
                    (mov (reg R11 ,loc) ,dst ,loc))]
                 ; second arg to cmp can't be a constant
                 [`(cmp ,src ,(? imm? dst) ,loc)
                  `((mov ,dst (reg R11 ,loc) ,loc)
                    (cmp ,src (reg R11 ,loc) ,loc))]
                 ; shift op count arg cannot be an address & the reg it operates on must be %cl
                 [`(,(? (contains? 'sar 'sal) kind) ,(? stack? how-many) ,what ,loc)
                  `((mov ,how-many (reg CX ,loc) ,loc)
                    (,kind (reg CX 1 ,loc) ,what ,loc))]
                 ; mov, add, sub, and, or, and xor cannot operate on two addresses
                 [`(,(? (contains? 'mov 'add 'sub 'cmp 'bitwise-and 'bitwise-or 'bitwise-xor) kind) ,(? stack? src) ,(? stack? dst) ,loc)
                  `((mov ,src (reg R10 ,loc) ,loc)
                    (,kind (reg R10 ,loc) ,dst ,loc))]
                 [x x])))

  (fix-invalid-movs (replace-vars (rewrite-operators tacky))))


(define (emit-assembly ast output-file)
  (define operand->string
    (match-lambda
      ; typed register. There is only one for now so we can support shifts
      [`(reg CX 1 ,_) "%cl"]
      ; untyped registers
      [`(reg AX ,_) "%eax"]
      [`(reg CX, _) "%ecx"]
      [`(reg DX ,_) "%edx"]
      [`(reg R10 ,_) "%r10d"]
      [`(reg R11 ,_) "%r11d"]
      [`(stack ,n ,_) (format "-~a(%rbp)" n)]
      [`(imm ,value ,_) (format "$~a" value)]))

  (define cond-jump->string
    (match-lambda
      ['jump-if-zero "je"]
      ['jump-if-not-zero "jne"]))

  (define comp-code->string
    (match-lambda
      ['equal "e"]
      ['not-equal "ne"]
      ['less-than "l"]
      ['less-or-equal "le"]
      ['greater-than "g"]
      ['greater-or-equal "ge"]))

  (define (label->string name)
    (format "L~a" name))

  (define instruction-map
    (hash 'mov "movl"
          'neg "negl"
          'not "notl"
          'add "addl"
          'sub "subl"
          'imul "imull"
          'idiv "idivl"
          'sar "sarl"
          'sal "sall"
          'cmp "cmpl"
          'bitwise-and "andl"
          'bitwise-xor "xorl"
          'bitwise-or "orl"))
  (define (instruction? i) (dict-has-key? instruction-map i))
  (define (instruction->string i) (dict-ref instruction-map i))

  (define (emit-binop op a b)
    (printf "    ~a ~a, ~a\n" op a b))

  (define (emit-unop op a)
    (printf "    ~a ~a\n" op a))

  (define (emit-label name)
    (printf "~a:\n" (label->string name)))

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
      [`(cdq ,_)
       (display "    cdq\n")]
      [`(jmp ,where ,_) (emit-unop "jmp" (label->string where))]
      [`(jmp-cc ,pred ,where ,_)
       (emit-unop (cond-jump->string pred) (label->string where))]
      [`(set-cc ,cc ,what ,_)
       (emit-unop (format "set~a" (comp-code->string cc)) (operand->string what))]
      [`(label ,name ,_) (emit-label name)]
      [`(,(? instruction? op) ,src ,dst ,_)
       (emit-binop (instruction->string op) (operand->string src) (operand->string dst))]
      [`(,(? instruction? op) ,value ,_)
       (emit-unop (instruction->string op) (operand->string value))]))

  (with-output-to-file output-file (λ () (emit ast)) #:exists 'replace))


(module+ main
  (rcc-compile "programs/return_2.c" 'assemble "programs/return_2.s"))
