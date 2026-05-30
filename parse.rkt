#lang racket

(require "utils.rkt" "schema.rkt")
(provide parse)


;; ensure-schema : ast? -> ast?
;; Validate that value conforms to the expected parse tree grammar.
;; Returns value unchanged, or raises an error if validation fails.
(define (ensure-schema value)
  ;; Recognize unary operator names.
  (define unary (schema-any 'negate 'complement 'not
                            'pre-increment 'pre-decrement 'post-increment 'post-decrement))
  ;; Recognize binary operator names.
  (define binary (schema-any 'add 'subtract 'multiply 'divide 'remainder
                             'and 'or ; these get removed in the next pass
                             'equal 'not-equal
                             'less-than 'less-or-equal 'greater-than 'greater-or-equal
                             'lshift 'rshift
                             'bitwise-and 'bitwise-xor 'bitwise-or
                             'assign
                             'add-assign 'sub-assign 'mult-assign 'div-assign 'rem-assign
                             'rshift-assign 'lshift-assign
                             'bit-and-assign 'bit-or-assign 'bit-xor-assign))
  ;; Schema for expression nodes (delayed for self-reference).
  (define expr
    (delay
      (schema-any
        (list binary expr expr span?)
        (list unary expr span?)
        (list 'conditional expr expr expr span?)
        (list 'var string? span?)
        (list 'int integer? span?))))
  ;; Schema for statements
  (define statement
    (delay
      (schema-any
       `(return ,expr ,span?)
       `(expression ,expr ,span?)
       `(if ,expr ,statement ,span?)
       `(if-else ,expr ,statement ,statement ,span?)
       `(compound ,block ,span?)
       `(break ,span?)
       `(continue ,span?)
       `(while ,expr ,statement ,span?)
       `(do-while ,statement ,expr ,span?)
       `(for ,block-item ,expr ,statement ,statement ,span?)
       `(switch ,expr ,statement ,span?)
       `(case ,expr ,statement ,span?)
       `(default ,statement ,span?)
       `(null ,span?)
       `(goto ,string? ,span?)
       `(label ,string? ,statement ,span?))))
  ;; Schema for declarations
  (define declaration
    (schema-any
     `(declare ,string? ,span?)
     `(declare-init ,string? ,expr ,span?)))
  ;; for loop init statement
  (define block-item (schema-any declaration statement))
  ;; Schema for blocks: a sequence of declarations and statements.
  (define block
    `(block ,(schema-many block-item)
            ,span?))
  ;; Schema for the top-level program node.
  (define program
    `(program (function "main" ,block ,span?) ,span?))
  (check-schema value program (schema-error-proc "Invalid parse tree" value))
  value)


;; fail : (or/c string? span?) string? ... -> void?
;; Raise a parser error with a source location and formatted message.
(define (fail loc . msg)
  (raise-user-error 'error "~a: parser: ~a" (format-loc loc) (apply format msg)))


;; A parser<A> is (list? -> (cons A list?)), returning (cons result remaining-tokens).

;; parse-sequence : parser<A> ... -> parser<(list A ...)>
;; Run each parser in sequence, collecting results into a list.
(define ((parse-sequence . parsers) tokens)
  (match parsers
    ['() (cons '() tokens)]
    [(cons parser rst)
     (match-let* ([(cons result remaining-tokens) (parser tokens)]
                  [(cons seq-results final-tokens) ((apply parse-sequence rst) remaining-tokens)])
       (cons (cons result seq-results) final-tokens))]))

;; map-p : (A -> B) parser<A> -> parser<B>
;; Apply fn to the result of parser.
(define ((map-p fn parser) tokens)
  (match (parser tokens)
    [(cons x tokens) (cons (fn x) tokens)]))

;; peek : [#:eof-error (or/c string? #f)] -> parser<(or/c token? #f)>
;; Return the next token without consuming it.
;; On EOF: return #f (default) or raise an error if #:eof-error is provided.
(define (peek #:eof-error [msg #f])
  (match-lambda
    ['() (if msg
             (fail "EOF" msg)
             (cons #f '()))]
    [(and tokens (cons tok _)) (cons tok tokens)]))

;; peek-sequence : integer? [#:eof-error (or/c string? #f)] -> parser<(or/c (listof token?) #f)>
;; Return the next length tokens without consuming them.
;; On short input: return #f (default) or raise an error if #:eof-error is provided.
(define ((peek-sequence length #:eof-error [msg #f]) tokens)
  (with-handlers ([exn:fail? (λ (_) (if msg (fail "EOF" msg) (cons #f tokens)))])
    (cons (take tokens length) tokens)))

;; then : parser<A> (A -> parser<B>) -> parser<B>
;; Monadic bind: run parser, pass its result to proc which returns a new parser.
(define ((then parser proc) tokens)
  (match (parser tokens)
    [(cons res tokens) ((proc res) tokens)]))

;; return : A -> parser<A>
;; Produce a value without consuming any tokens.
(define ((return what) tokens)
  (cons what tokens))

;; expect-kind : symbol? -> parser<token?>
;; Match a token with the given kind, or fail.
(define ((expect-kind kind) tokens)
  (match tokens
    ['() (fail "EOF" "Expected ~a, but reached end of input" kind)]
    [`((,(== kind) ,_ ,_) ,@_) tokens]
    [`((,actual-kind ,_ ,loc) ,@_)
     (fail loc "Expected ~a, but found ~a" kind actual-kind)]))

;; expect : symbol? any/c -> parser<token?>
;; Match a token with the given kind and value, or fail.
(define ((expect kind value) tokens)
  (match tokens
    ['() (fail "EOF" "Expected ~a ~a, but reached end of input" kind value)]
    [`((,(== kind) ,(== value) ,_) ,@_) tokens]
    [`((,actual-kind ,actual-value ,loc) ,@_)
     (fail loc "Expected ~a ~a, but found ~a ~a" kind value actual-kind actual-value)]))

;; any-token : parser<token?>
;; Match any single token, or fail at EOF.
(define (any-token tokens)
  (match tokens
    ['() (fail "EOF" "Expected any token, but reached end of input")]
    [_ tokens]))


;; prefix : string? symbol? -> symbol?
;; Prepend a string to a symbol name, producing a new symbol.
(define (prefix what symbol)
  (string->symbol (format "~a~a" what symbol)))


;; parse-postfix : expr? -> parser<expr?>
;; Consume any postfix increment/decrement operators following a parsed expression.
(define (parse-postfix factor)
  ((peek)
   . then .
   (match-lambda
     [`(,(and kind (or 'increment 'decrement)) ,_ ,end-loc)
      (let ([node `(,(prefix "post-" kind) ,factor ,(join-locs (last factor) end-loc))])
        ((map-p (const node) any-token) . then . parse-postfix))]
     [_ (return factor)])))


;; parse-factor : parser<expr?>
;; Parse a primary expression: integer literal, variable, unary op, or parenthesized expression.
(define parse-factor
  ((any-token
   . then .
   (match-lambda
     [`(const ,value ,loc)
      (return `(int ,(string->number value) ,loc))]
     [`(ident ,name ,loc)
      (return `(var ,name ,loc))]
     [`(,(and kind (or 'complement 'negate 'not 'increment 'decrement)) ,_ ,loc)
      (let ([op (if (member kind '(increment decrement)) (prefix "pre-" kind) kind)])
        (map-p (λ (fact) `(,op ,fact ,(join-locs loc fact)))
               parse-factor))]
     [`(lparen ,_ ,_)
      (map-p (match-lambda [(list e _) e])
             (parse-sequence (parse-expr 0) (expect-kind 'rparen)))]
     [`(,kind ,value ,loc)
      (fail loc "Expected expression, but found ~a ~a" kind value)]))
   . then .
   parse-postfix))


;; parse-expr : integer? -> parser<expr?>
;; Parse a binary expression using precedence climbing. Parses a left-hand
;; operand with parse-factor, then iterates: if the next token is an operator
;; with precedence >= min-prec, consume it and recurse at (prec + 1) for the
;; right operand to get left-associativity. Assignment-family operators recurse at (prec) instead
;; for right-associativity.
(define ((parse-expr min-prec) t1)
  ;; precedence : (hash/c symbol? integer?)
  ;; Map from operator token kinds to their precedence levels.
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
                           'or               5
                           'question         3
                           'assign           1
                           'add-assign       1
                           'sub-assign       1
                           'mult-assign      1
                           'div-assign       1
                           'rem-assign       1
                           'rshift-assign    1
                           'lshift-assign    1
                           'bit-and-assign   1
                           'bit-or-assign    1
                           'bit-xor-assign   1
                           ))

  (define assign-op?
    (contains?
     'assign
     'add-assign
     'sub-assign
     'mult-assign
     'div-assign
     'rem-assign
     'rshift-assign
     'lshift-assign
     'bit-and-assign
     'bit-or-assign
     'bit-xor-assign))

  ;; parse-conditional-middle : parser<expr?>
  ;; Parse the middle operand of a ternary expression, up to and including the colon.
  (define parse-conditional-middle
    (map-p car (parse-sequence (parse-expr 0) (expect-kind 'colon))))

  ;; valid-operator? : token? -> boolean?
  ;; True if the token is an operator with precedence >= min-prec.
  (define (valid-operator? token)
    ((hash-ref precedence (first token) -1) . >= . min-prec))

  ;; rec : expr? -> parser<expr?>
  ;; Iteratively consume binary operators, building up a left-nested AST.
  (define ((rec left) t1)
    (match t1
      [(cons (? valid-operator? `(,(? assign-op? kind) ,_ ,_)) t2)
       (match-let* ([op-prec (hash-ref precedence kind)]
                    [(cons right t3) ((parse-expr op-prec) t2)]
                    [new-left `(,kind ,left ,right ,(join-locs left right))])
         ((rec new-left) t3))]
      [(cons (? valid-operator? `(question ,_ ,_)) t2)
       (match-let* ([op-prec (hash-ref precedence 'question)]
                    [(cons middle t3) (parse-conditional-middle t2)]
                    [(cons right t4) ((parse-expr op-prec) t3)]
                    [new-left `(conditional ,left ,middle ,right ,(join-locs left right))])
         ((rec new-left) t4))]
      [(cons (? valid-operator? op-token) t2)
       (match-let* ([`(,op ,_ ,_) op-token]
                    [op-prec (hash-ref precedence op)]
                    [(cons right t3) ((parse-expr (+ 1 op-prec)) t2)]
                    [new-left `(,op ,left ,right ,(join-locs left right))])
         ((rec new-left) t3))]
      [_ (cons left t1)]))

  (match-let ([(cons left t2) (parse-factor t1)])
    ((rec left) t2)))


;; parse-for-init : parser<(or/c declaration? statement?)>
;; Parse the initializer clause of a for loop. Consumes the trailing semicolon.
;; Returns a declaration when the clause begins with `int`, a null statement
;; when the clause is empty, or an expression statement otherwise.
(define parse-for-init
  ((peek)
   . then .
   (match-lambda
     [`(keyword int ,_) parse-declaration]
     [`(semicolon ,_ ,loc) (map-p (const `(null ,loc)) any-token)]
     [_ (map-p (match-lambda [`(,expr ,end) `(expression ,expr ,(join-locs expr end))])
               (parse-sequence (parse-expr 0) (expect-kind 'semicolon)))])))

;; parse-for-control : parser<expr?>
;; Parse the loop-condition clause of a for loop. Consumes the trailing semicolon.
;; Returns the parsed expression, or the constant 1 if the clause is empty.
(define parse-for-control
  ((peek)
   . then .
   (match-lambda
     [`(semicolon ,_ ,loc) (map-p (const `(int 1 ,loc)) any-token)]
     [_ (map-p first (parse-sequence (parse-expr 0) (expect-kind 'semicolon)))])))

;; parse-for-final : parser<statement?>
;; Parse the post-iteration clause of a for loop. Does NOT consume the closing
;; paren — the caller handles that. Returns a null statement if the clause is
;; empty, or an expression statement wrapping the parsed expression.
(define parse-for-final
  ((peek)
   . then .
   (match-lambda
     [`(rparen ,_ ,loc) (return `(null ,loc))]
     [_ (map-p (λ (expr) `(expression ,expr ,(last expr)))
               (parse-expr 0))])))

;; parse-statement : parser<statement?>
;; Parse one C statement. Dispatches on the leading token(s) to handle:
;; return, null (`;`), goto, labeled statements (`ident :`), compound blocks,
;; break, continue, while, do-while, for, if/if-else, and expression statements.
(define parse-statement
  ;; label statements start with an identifier and then a colon, so we need to see two tokens to know
  ;; if we should parse a label or a regular expression statement.
  ((peek-sequence 2 #:eof-error "Expecting statement but reached end of input")
   . then .
   (match-lambda
     [`((keyword return ,_) ,_)
      (map-p (match-lambda [(list start expr end) `(return ,expr ,(join-locs start end))])
             (parse-sequence any-token (parse-expr 0) (expect-kind 'semicolon)))]
     [`((semicolon ,_ ,loc) ,_)
      (map-p (const `(null ,loc)) any-token)]
     [`((keyword goto ,_) ,_)
      (map-p (match-lambda [`(,start (ident ,name ,_) ,end) `(goto ,name ,(join-locs start end))])
             (parse-sequence any-token (expect-kind 'ident) (expect-kind 'semicolon)))]
     ;; maybe a labeled statement. Check for a colon
     [`((ident ,name ,start) (colon ,_ ,_))
      (map-p (match-lambda [`(,_ ,_ ,stmt) `(label ,name ,stmt ,(join-locs start stmt))])
             (parse-sequence any-token any-token parse-statement))]
     [`((lbrace ,_ ,_) ,_) (map-p (λ (block) `(compound ,block ,(last block))) parse-block)]
     [`((keyword break ,loc) ,_)
      (map-p (const `(break ,loc))
             (parse-sequence any-token (expect-kind 'semicolon)))]
     [`((keyword continue ,loc) ,_)
      (map-p (const `(continue ,loc))
             (parse-sequence any-token (expect-kind 'semicolon)))]
     [`((keyword while ,_) ,_)
      (map-p (match-lambda [`(,start ,_ ,condition ,_ ,body)
                            `(while ,condition ,body ,(join-locs start body))])
             (parse-sequence any-token
                             (expect-kind 'lparen)
                             (parse-expr 0)
                             (expect-kind 'rparen)
                             parse-statement))]
     [`((keyword do ,_) ,_)
      (map-p (match-lambda [`(,start ,body ,_ ,_ ,condition ,_ ,end)
                            `(do-while ,body ,condition ,(join-locs start end))])
             (parse-sequence any-token
                             parse-statement
                             (expect 'keyword 'while)
                             (expect-kind 'lparen)
                             (parse-expr 0)
                             (expect-kind 'rparen)
                             (expect-kind 'semicolon)))]
     [`((keyword for ,_) ,_)
      (map-p (match-lambda [`(,start ,_ ,initial ,control ,final ,_ ,body)
                            `(for ,initial ,control ,final ,body ,(join-locs start body))])
             (parse-sequence any-token
                             (expect-kind 'lparen)
                             parse-for-init
                             parse-for-control
                             parse-for-final
                             (expect-kind 'rparen)
                             parse-statement))]
     [`((keyword switch ,_) ,_)
      (map-p (match-lambda [`(,start ,_ ,value ,_ ,body)
                            `(switch ,value ,body ,(join-locs start body))])
             (parse-sequence any-token
                             (expect-kind 'lparen)
                             (parse-expr 0)
                             (expect-kind 'rparen)
                             parse-statement))]
     [`((keyword case ,_) ,_)
      (map-p (match-lambda [`(,start ,value ,_ ,body) `(case ,value ,body ,(join-locs start body))])
             (parse-sequence any-token (parse-expr 0) (expect-kind 'colon) parse-statement))]
     [`((keyword default ,_) ,_)
      (map-p (match-lambda [`(,start ,_ ,body) `(default ,body ,(join-locs start body))])
             (parse-sequence any-token (expect-kind 'colon) parse-statement))]
     [`((keyword if ,_) ,_)
      ((parse-sequence any-token
                       (expect-kind 'lparen)
                       (parse-expr 0)
                       (expect-kind 'rparen)
                       parse-statement
                       (peek))
       . then .
       (match-lambda
         [`(,keyword ,_ ,expr ,_ ,body (keyword else ,_))
          (map-p (match-lambda [`(,_ ,else-body)
                                `(if-else ,expr ,body ,else-body ,(join-locs keyword else-body))])
                 (parse-sequence any-token parse-statement))]
         [`(,keyword ,_ ,expr ,_ ,body ,_)
          (let ([loc (join-locs keyword body)])
            (return `(if ,expr ,body ,loc)))]))]
     [_
      (map-p (match-lambda [(list e end) `(expression ,e ,(join-locs e end))])
             (parse-sequence (parse-expr 0) (expect-kind 'semicolon)))])))


;; parse-declaration : parser<declaration?>
;; Parse a variable declaration, with or without an initializer.
(define parse-declaration
  ((parse-sequence (expect 'keyword 'int) (expect-kind 'ident) any-token)
   . then .
   (match-lambda
     [`(,start (ident ,name ,_) (semicolon ,_ ,end))
      (return `(declare ,name ,(join-locs start end)))]
     [`(,start (ident ,name ,_) (assign ,_ ,_))
      (map-p (match-lambda [(list e end) `(declare-init ,name ,e ,(join-locs start end))])
             (parse-sequence (parse-expr 0) (expect-kind 'semicolon)))]
     [`(,_ ,_ (,kind ,value ,loc))
      (fail loc "Expecting ; or =, but found ~a ~a" kind value)])))


;; parse-block-items : parser<(listof block-item?)>
;; Recursively parse block items until a closing brace is reached.
(define parse-block-items
  ((peek #:eof-error "Expecting statement or } but reached end of input")
   . then .
   (match-lambda
     [`(rbrace ,_ ,_)
      (return '())]
     [`(keyword int ,_)
      (map-p (match-lambda [(list dec rst) (cons dec rst)])
             (parse-sequence parse-declaration parse-block-items))]
     [_
      (map-p (match-lambda [(list statement rst) (cons statement rst)])
             (parse-sequence parse-statement parse-block-items))])))


;; parse-block : parser<block?>
;; Parse a brace-delimited block: { item ... }.
(define parse-block
  (map-p (match-lambda [`(,start ,body ,end) `(block ,body ,(join-locs start end))])
         (parse-sequence (expect-kind 'lbrace) parse-block-items (expect-kind 'rbrace))))


;; parse-function : parser<function?>
;; Parse a function definition: int name(void) { body }.
(define parse-function
  (map-p
   (match-lambda
     [`(,start (ident ,name ,_) ,_ ,_ ,_ (block ,body ,end))
      (let ([loc (join-locs start end)])
        `(function ,name (block ,(append body `((return (int 0 ,loc) ,loc))) ,end) ,loc))])
   (parse-sequence
    (expect 'keyword 'int)
    (expect-kind 'ident)
    (expect-kind 'lparen)
    (expect 'keyword 'void)
    (expect-kind 'rparen)
    parse-block)))


;; parse-program : parser<program?>
;; Parse a top-level program consisting of a single function.
(define parse-program
  (map-p
   (match-lambda [(and f `(function ,_ ,_ ,loc)) `(program ,f ,loc)])
   parse-function))


;; rename-subtract : ast? -> ast?
;; - Rewrite (negate a b loc) to (subtract a b loc) throughout the tree,
;;   distinguishing binary subtraction from unary negation.
;; - Fold (negate (int n)) to (int -n)
(define rename-subtract
  (bottom-up
   (match-lambda
     [`(negate ,a ,b ,loc) `(subtract ,a ,b ,loc)]
     [`(negate (int ,n ,_) ,loc) `(int ,(- n) ,loc)]
     [x x])))


;; parse : (listof token?) -> program?
;; Parse a token list into a validated AST.
(define (parse tokens)
  (ensure-schema
    (match (rename-subtract (parse-program tokens))
      [(cons prog '()) prog]
      [(cons _ `((,kind ,value ,loc) ,@_))
       (fail loc "Expected EOF, but found ~a ~a" kind value)])))
