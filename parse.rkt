#lang racket

(require "utils.rkt" "schema.rkt")
(provide parse)


(define (ensure-schema value)
  (define named?
    (contains? 'lparen 'rparen 'lbrace 'rbrace 'semicolon
               'ident 'const
               'complement 'not
               'multiply 'divide 'remainder
               'add 'negate
               'lshift 'rshift
               'less-than 'less-or-equal 'greater-than 'greater-or-equal 'equal 'not-equal
               'bitwise-and 'bitwise-xor 'bitwise-or
               'and 'or))
  (define keywd? (contains? 'return 'void 'int))
  (define token
    (schema-any
      (list 'keyword keywd? span?)
      (list named? string? span?)))
  (define (err bad-value schema)
    (raise-user-error 'invalid-parser-input "~v doesn't match ~v in ~v" bad-value schema value))
  (check-schema value (schema-many token) err))


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


(define (parse tokens)
  (ensure-schema tokens)
  (match (rename-subtract (parse-program tokens))
    [(cons prog '()) prog]
    [(cons _ `((,kind ,value ,loc) ,@_))
     (raise-user-error 'error "~a: paser: Expected eof, but found ~a ~a" (format-loc loc) kind value)]))
