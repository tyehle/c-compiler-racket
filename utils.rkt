#lang racket


(provide debug
         (struct-out span)
         format-srcloc format-loc join-locs
         member? contains?
         walk-tree top-down bottom-up)


(struct span (path start-line start-col stop-line stop-col))


(define (debug label . vs)
  (printf "\n~a\n" label)
  (for ([v vs]) (pretty-print v))
  (car vs))


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


(module+ main
  '())
