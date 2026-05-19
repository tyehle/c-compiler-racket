#lang racket


(provide debug
         (struct-out span)
         format-loc join-locs
         member? contains?
         top-down bottom-up contextual-top-down)


;; Source location spanning from start to stop.
(struct span (path start-line start-col stop-line stop-col))


;; debug : any/c any/c ... -> any/c
;; Print label and values, then return the first value.
(define (debug label . vs)
  (printf "\n~a\n" label)
  (for ([v vs]) (pretty-print v))
  (car vs))


;; format-loc : span? -> string?
;; Format a source location as "path:line:col".
(define format-loc (match-lambda
                     [(span name line col _ _) (format "~a:~a:~a" name line col)]))


;; join-locs : any/c any/c -> span?
;; Combine two source locations into a span covering both. If either argument
;; is an AST node (a list ending with a span), extract its span first.
(define (join-locs a b)
  (match (cons a b)
    [(cons `(,@_ ,(and a (? span?))) b) (join-locs a b)]
    [(cons a `(,@_ ,(and b (? span?)))) (join-locs a b)]
    [(cons (span path start-line start-col _ _) (span _ _ _ end-line end-col))
     (span path start-line start-col end-line end-col)]))


;; member? : any/c list? -> boolean?
;; True if x is equal? to any element of xs.
(define (member? x xs)
  (match xs
    ['() #f]
    [(cons fst rst) (or (equal? fst x) (member? x rst))]))


;; contains? : any/c ... -> (any/c -> boolean?)
;; Return a predicate that tests membership in xs.
(define ((contains? . xs) x)
  (member? x xs))


;; walk-tree : (any/c -> any/c) any/c -> any/c
;; Apply fn to each child of an AST node, preserving the node's kind and span.
;; For plain lists, splice results that are not AST nodes (symbol-headed lists).
(define (walk-tree fn tree)
  ;; splice-nodes : list? -> list?
  ;; Flatten non-AST sublists while keeping AST nodes intact.
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

;; contextual-top-down : C (C T -> (cons C T)) -> (T -> T)
;; where C and T are unconstrained — any value may serve as the context and
;; any value as the tree.
;; Top-down walk that threads a context down the tree. fn receives the current
;; context and node, and returns (cons new-context new-node): the new context
;; is passed to fn for the children, and the new node replaces the original.
;; Each subtree is walked with the context produced by its parent's call, so
;; updates only propagate downward — sibling subtrees do not see each other's
;; context changes.
(define ((contextual-top-down context fn) tree)
  (match-let ([(cons new-context new-tree) (fn context tree)])
    (walk-tree (contextual-top-down new-context fn) new-tree)))


;; top-down : (any/c -> any/c) -> (any/c -> any/c)
;; Apply fn to each node before recursing into children.
(define ((top-down fn) tree)
  (walk-tree (top-down fn) (fn tree)))


;; bottom-up : (any/c -> any/c) -> (any/c -> any/c)
;; Recurse into children before applying fn to each node.
(define ((bottom-up fn) tree)
  (fn (walk-tree (bottom-up fn) tree)))


;; generic-walk : (any/c -> any/c) (any/c -> any/c) -> (any/c -> any/c)
;; Apply before to each node on the way down and after on the way up.
;; Generalizes top-down and bottom-up; useful when entering and leaving a
;; subtree both need to run effects (e.g. push/pop a scope).
(define ((generic-walk before after) tree)
  (after (walk-tree (generic-walk before after) (before tree))))
