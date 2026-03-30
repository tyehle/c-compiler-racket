#lang racket

(provide rcc-compile)


; --- Source location tracking ---

(struct source-span (path start-line start-col stop-line stop-col))

; --- AST node types ---

(struct token (kind value loc) #:transparent)

(struct program (definition loc) #:transparent)
(struct function (name body loc) #:transparent)
(struct statement (value loc) #:transparent)
(struct expr (value loc) #:transparent)

(struct instruction (value loc) #:transparent)
(struct operand (value loc) #:transparent)


; debug : symbol any -> any
; Prints a labeled value and returns it, for use in debugging pipelines.
(define (debug symb v)
  (printf "~v: " symb)
  (pretty-print v)
  v)


; format-srcloc : string input-port -> string
; Formats the port's current position as "file:line:col" or "file:offset".
(define (format-srcloc name file)
  (match/values (port-next-location file)
                [(#f #f n) (format "~a:~a" name n)]
                [(line col _) (format "~a:~a:~a" name line col)]))

; format-loc : source-span -> string
; Formats the start position of a source span as "file:line:col".
(define (format-loc span)
  (match span
    [(source-span name line col _ _) (format "~a:~a:~a" name line col)]))


; concat-srclocs : source-span source-span -> source-span
; Combines the start of loc-start with the end of loc-end into a single span.
(define (concat-srclocs loc-start loc-end)
  (match-let ([(source-span path start-line start-col _ _) loc-start]
              [(source-span _ _ _ end-line end-col) loc-end])
    (source-span path start-line start-col end-line end-col)))


; fail : (or/c source-span string) string string any ... -> void
; Raises a user error with a formatted message, accepting either a source-span
; or a pre-formatted location string.
(define (fail loc stage . msg)
  (cond
    [(source-span? loc)
     (raise-user-error 'error "~a: ~a: ~a" (format-loc loc) stage (apply format msg))]
    [else
     (raise-user-error 'error "~a: ~a: ~a" loc stage (apply format msg))]))


; rcc-compile : string symbol string -> void
; Main entry point. Runs the compiler pipeline up to the stage specified by mode:
; 'lex, 'parse, 'tacky, 'codegen, 'assemble, or 'full.
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


; walk-tree : (AST-node -> AST-node) AST-node -> AST-node
; Applies fn to each immediate child of tree, preserving the node structure.
; Does not recurse — use top-down or bottom-up for recursive traversal.
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


; top-down : (AST-node -> AST-node) -> (AST-node -> AST-node)
; Returns a function that transforms a tree by applying fn before recursing into children.
(define ((top-down fn) tree)
  (walk-tree (top-down fn) (fn tree)))


; bottom-up : (AST-node -> AST-node) -> (AST-node -> AST-node)
; Returns a function that transforms a tree by recursing into children before applying fn.
(define ((bottom-up fn) tree)
  (fn (walk-tree (bottom-up fn) tree)))


; lex : string -> (listof token)
; Reads the file at input-file-name and returns a flat list of tokens.
(define (lex input-file-name)
  (call-with-input-file input-file-name
    (λ (in) 'body
      (port-count-lines! in)

      ; decode-match : symbol (cons natural natural) -> (listof bytes) -> (listof token)
      ; Curried helper for use with cond's => syntax. Given a token kind and start
      ; position, returns a function that converts a regexp match result into a token
      ; and continues lexing.
      (define ((decode-match symb start) data)
        (let*-values ([(value) (bytes->string/utf-8 (car data))]
                      [(row col _) (port-next-location in)]
                      [(loc) (source-span input-file-name (car start) (cdr start) row col)]
                      [(decoded) (token symb value loc)])
          ; (println decoded)
          (cons decoded (go))))

      ; go : -> (listof token)
      ; Recursively consumes input from the port, matching one token per call.
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

            [else (fail
                   (format-srcloc input-file-name in)
                   "lexer"
                   "Unrecognized token ~a"
                   (peek-char in))])))

      ; replace-keywords : (listof token) -> (listof token)
      ; Converts identifier tokens whose values are reserved words into keyword tokens.
      (define (replace-keywords tokens)
        (map
         (match-lambda
           [(token 'ident v loc)
            #:when (member v '("int" "void" "return"))
            (token 'keyword (string->symbol v) loc)]
           [t t])
         tokens))

      (replace-keywords (go)))))

; parser : (listof token) -> program
; Parses a flat list of tokens into an AST rooted at a program struct,
; or raises a user error if the token stream does not match the grammar.
(define (parse tokens)
  ; a parser<A> is: (listof token) -> (cons A (listof token))

  ; parse-sequence : parser<A> ... -> parser<(listof A)>
  ; Combines zero or more parsers in sequence, returning a parser that produces
  ; a list of each sub-parser's result and the remaining tokens after all parsers run.
  (define ((parse-sequence . parsers) tokens)
    (match parsers
      ['() (cons '() tokens)]
      [(cons parser rst)
       (match-let* ([(cons result remaining-tokens) (parser tokens)]
                    [(cons seq-results final-tokens) ((apply parse-sequence rst) remaining-tokens)])
         (cons (cons result seq-results) final-tokens))]))

  ; map/p : (A -> B) parser<A> -> parser<B>
  ; Returns a new parser that applies fn to the result of parser, leaving remaining tokens unchanged.
  (define ((map/p fn parser) tokens)
    (match (parser tokens)
      [(cons x tokens) (cons (fn x) tokens)]))

  ; satisfies : string (token -> boolean) -> parser<token>
  ; Consumes the next token if pred holds, or raises a user error using name.
  (define ((satisfies name pred) tokens)
    (match tokens
      ['() (fail "parser" "Expected ~a, but reached end of input" name)]
      [(cons fst rst) #:when (pred fst) (cons fst rst)]
      [(cons (token kind value loc) _)
       (fail loc "parser" "Expected ~a, but found ~a ~a" name kind value)]))

  ; expect-kind : symbol -> parser<token>
  ; Consumes the next token if its kind matches, or raises a user error.
  (define (expect-kind kind)
    (satisfies kind (λ (t) (eq? kind (token-kind t)))))
  ; expect : symbol (or/c string symbol) -> parser<token>
  ; Consumes the next token if both its kind and value match, or raises a user error.
  (define (expect kind value)
    (satisfies (format "~a ~a" kind value)
               (λ (t) (and (eq? kind (token-kind t))
                           (eq? value (token-value t))))))
  ; any-token : parser<token>
  ; Consumes and returns the next token unconditionally.
  (define any-token (satisfies "anything" (const #t)))

  ; parse-expr : parser<expr>
  ; Parses a unary expression: an integer constant, negate/complement applied to an expr,
  ; or a parenthesized expr.
  (define (parse-expr tokens)
    (define const-parser
      (map/p
        (match-lambda [(token _ value loc)
                       (expr `(int ,(string->number value)) loc)])
        any-token))
    (define (unop-parser symbol)
      (map/p
        (match-lambda [(list t e)
                       (expr (list symbol e)
                             (concat-srclocs (token-loc t) (expr-loc e)))])
        (parse-sequence any-token parse-expr)))
    (define paren-parser
      (map/p
        (match-lambda [(list _ e _) e])
        (parse-sequence any-token parse-expr (expect-kind 'rparen))))

    (match tokens
      [(cons (token 'const _ _) _) (const-parser tokens)]
      [(cons (token 'negate _ _) _) ((unop-parser 'negate) tokens)]
      [(cons (token 'complement _ _) _) ((unop-parser 'complement) tokens)]
      [(cons (token 'lparen _ _) _) (paren-parser tokens)]
      [(cons (token kind val loc) _)
       (fail loc "parser" "Expected an expression, but found ~a ~a" kind val)]
      ['() (fail "EOF" "parser" "Expected an expression")]))

  ; parse-statement : parser<statement>
  ; Parses a return statement of the form: return <expr> ;
  (define parse-statement
    (map/p
     (match-lambda
       [(list (token _ _ loc-start) expr (token _ _ loc-end))
        (statement `(return ,expr) (concat-srclocs loc-start loc-end))])
     (parse-sequence (expect 'keyword 'return) parse-expr (expect-kind 'semicolon))))

  ; parse-function : parser<function>
  ; Parses a function definition of the form: int <ident> ( void ) { <statement> }
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

  ; parse-program : parser<program>
  ; Parses a complete program consisting of a single function definition.
  (define parse-program
    (map/p
     (λ (function) (program function (function-loc function)))
     parse-function))

  (match (parse-program tokens)
    [(cons prog '()) prog]
    [(cons _ (cons (token kind value loc) _))
     (fail loc "parser" "Expected eof, but found ~a ~a" kind value)]))

; gen-tacky : program -> program
; Transforms a parsed program into the tacky ir
(define (gen-tacky prog)
  ; next-tacky-var : natural
  ; Counter for generating unique temporary variable names.
  (define next-tacky-var 0)
  ; fresh-tacky-tmp-var : source-span -> operand
  ; Generates a fresh temporary variable operand with a unique name.
  (define (fresh-tacky-tmp-var loc)
    (let ([name (format "tmp.~a" next-tacky-var)])
      (set! next-tacky-var (add1 next-tacky-var))
      (operand `(var ,name) loc)))

  ; unary? : symbol -> (or/c list #f)
  ; Returns a truthy value if expr-kind is a unary operator.
  (define (unary? expr-kind)
    (member expr-kind '(negate complement)))

  ; transform : AST-node -> AST-node
  ; Rewrites AST nodes into tacky IR: converts integer literals to immediate
  ; operands, unary expressions to instruction sequences with fresh temporaries,
  ; and return statements to flat instruction lists.
  (define transform
    (match-lambda
      ; transform base instructions into (cons operand instruction-list) form. This represents the
      ; destination variable and all the instructions required to produce compute the value stored
      ; in that variable
      [(expr `(int ,n) loc) (list (operand `(imm ,n) loc))]
      ; this is a bottom up transform, so all sub expressions will be (cons operand instruction-list)
      [(expr `(,(? unary? kind) (,op ,@instructions)) loc)
       (let* ([dest (fresh-tacky-tmp-var loc)]
              [instr (instruction (list kind op dest) loc)])
         (cons dest (cons instr instructions)))]
      ; above cases should handle all instructions
      [(expr val loc) (fail loc "tacky" "Unknown expr: ~a" val)]
      ; the instruction list is in reverse order because we constructed it bottom-up
      [(statement `(return (,(? operand? v) ,@instructions)) loc)
       (reverse (cons (instruction `(return ,v) loc) instructions))]
      ; that should be all the statements
      [(statement val loc) (fail loc "tacky" "Unknown statement ~a" val)]
      [x x]))

  ((bottom-up transform) prog))


; assemble : program -> program
; Transforms tacky IR into x86-style assembly IR in three passes:
; rewrite operators, replace variables with stack slots, and fix invalid movs.
(define (assemble tacky)
  ; unary? : symbol -> (or/c list #f)
  ; Returns a truthy value if kind is a unary operator.
  (define (unary? kind) (member kind '(negate complement)))

  ; convert-instruction-kind : symbol -> symbol
  ; Maps tacky operator names to their x86 assembly equivalents.
  (define convert-instruction-kind
    (match-lambda
      ['complement 'not]
      ['negate 'neg]))

  ; flatten-instruction-list : (treeof instruction) -> (listof instruction)
  ; Flattens arbitrarily nested instruction lists into a flat list.
  (define flatten-instruction-list
    (match-lambda
      ['() '()]
      [(cons (and fst `(,(? symbol?) ,@_)) rst) (cons fst (flatten-instruction-list rst))]
      [(cons (? instruction? fst) rst) (cons fst (flatten-instruction-list rst))]
      [(cons fst rst) (append (flatten-instruction-list fst) (flatten-instruction-list rst))]))

  ; rewrite-operators : program -> program
  ; Expands tacky instructions into x86 instruction sequences: return becomes
  ; mov+ret, and unary ops become mov+op. Flattens resulting nested lists.
  (define rewrite-operators
    (bottom-up (match-lambda
                 [(instruction `(return ,op) loc)
                  (list (instruction `(mov ,op ,(operand `(reg AX) loc)) loc)
                        (instruction `(ret) loc))]
                 [(instruction `(,(? unary? kind) ,src ,dst) loc)
                  (list (instruction `(mov ,src ,dst) loc)
                        (instruction `(,(convert-instruction-kind kind) ,dst) loc))]
                 ; that should be all the instructions
                 [(instruction val loc) (fail loc "assembler" "Unknown instruction ~a" val)]
                 [(function name is loc) (function name (flatten-instruction-list is) loc)]
                 [x x])))

  ; stack-offset : natural
  ; Tracks the current stack frame size for variable allocation.
  (define stack-offset 0)
  ; next-stack-offset : -> natural
  ; Advances the stack offset by 4 bytes and returns the new offset.
  (define (next-stack-offset)
    (set! stack-offset (+ 4 stack-offset))
    stack-offset)
  ; var-map : (hash/c string natural)
  ; Maps variable names to their assigned stack offsets.
  (define var-map (make-hash))
  ; replace-vars : program -> program
  ; Replaces (var name) operands with (stack offset) operands, assigning each
  ; variable a unique stack slot. Prepends an allocate-stack instruction to
  ; each function with the total frame size, or omits it if no variables exist.
  (define replace-vars
    (bottom-up (match-lambda
                 [(operand `(var ,name) loc)
                  (let ([offset (hash-ref! var-map name next-stack-offset)])
                    (operand `(stack ,offset) loc))]
                 [(function name is loc)
                  (let* ([alloc-instr (instruction `(allocate-stack ,stack-offset) loc)]
                         [body (if (eq? stack-offset 0) is (cons alloc-instr is))])
                    (set! stack-offset 0)
                    (set! var-map (make-hash))
                    (function name body loc))]
                 [x x])))

  ; stack? : operand -> boolean
  ; Returns #t if the operand is a stack reference.
  (define (stack? op) (equal? 'stack (car (operand-value op))))
  ; fix-invalid-movs : program -> program
  ; Splits mov instructions where both src and dst are stack operands into
  ; two movs via a temporary register (R10), since x86 disallows memory-to-memory mov.
  (define fix-invalid-movs
    (bottom-up (match-lambda
                 [(instruction `(mov ,(? stack? src) ,(? stack? dst)) loc)
                  (let ([tmp-reg (operand `(reg R10) loc)])
                    (list (instruction `(mov ,src ,tmp-reg) loc)
                          (instruction `(mov ,tmp-reg ,dst) loc)))]
                 [(function name is loc) (function name (flatten-instruction-list is) loc)]
                 [x x])))

  (fix-invalid-movs (replace-vars (rewrite-operators tacky))))


; emit-assembly : program string -> void
; Writes x86-64 assembly text for the given program AST to output-file.
(define (emit-assembly ast output-file)
  ; operand->string : operand -> string
  ; Converts an operand to its AT&T syntax assembly representation.
  (define (operand->string o)
    (match o
      [(operand `(reg AX) _) "%eax"]
      [(operand `(reg R10) _) "%r10d"]
      [(operand `(stack ,n) _) (format "-~a(%rbp)" n)]
      [(operand `(imm ,value) _) (format "$~a" value)]))

  ; ikind->string : symbol -> string
  ; Maps instruction kind symbols to their AT&T syntax mnemonics.
  (define (ikind->string s)
    (match s
      ['mov "movl"]
      ['neg "negl"]
      ['not "notl"]))

  ; emit-binop : string string string -> void
  ; Prints a two-operand assembly instruction.
  (define (emit-binop op a b)
    (printf "    ~a ~a, ~a\n" op a b))

  ; emit-unop : string string -> void
  ; Prints a one-operand assembly instruction.
  (define (emit-unop op a)
    (printf "    ~a ~a\n" op a))

  ; emit-instruction : instruction -> void
  ; Prints the assembly for a single instruction.
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

  ; emit-function : function -> void
  ; Prints the assembly for a function: .globl directive, label, prologue, and body.
  (define (emit-function fn)
    (match-let ([(function src-name body _) fn])
      (let ([name (string-append "_" src-name)]) ; leading underscore is macOS/Mach-O convention
        (printf "    .globl ~a\n~a:\n" name name)
        (emit-unop "pushq" "%rbp")
        (emit-binop "movq" "%rsp" "%rbp")
        (for ([i body])
          (emit-instruction i)))))

  ; emit-program : program -> void
  ; Prints the assembly for a complete program.
  (define (emit-program ast)
    (match-let ([(program fn _) ast])
      (emit-function fn)))

  (with-output-to-file output-file (λ () (emit-program ast)) #:exists 'replace))


(module+ main
  (rcc-compile "programs/return_2.c" 'assemble "programs/return_2.s"))
