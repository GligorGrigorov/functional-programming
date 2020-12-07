(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define head car)
(define tail cdr)
(define visualize-t '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
(define (remove-spaces str)
  (list->string (filter (lambda (x) (not (char=? #\space x))) (string->list str)))
  )
(define (next x) (+ x 1))

(define (index-of char str)
  (define (helper i)
    (if (< i (string-length str))
        (if (char=? char (string-ref str i))
            i
            (helper (next i))
            )
        -1
        )
    )
  (helper 0)
  )

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (find-closing-bracket str)
  (define (helper lst count pos)
    (cond
      [(null? lst) pos]
      [(and (not (= 0 pos)) (= count 0)) pos]
      [(char=? #\{ (car lst)) (helper (cdr lst) (1+ count) (1+ pos))]
      [(char=? #\} (car lst)) (helper (cdr lst) (1- count) (1+ pos))]
      [else (helper (cdr lst) count (1+ pos))]
      )
    )
  (helper (string->list str) 0 0)
  )
(define (number? expr)
  (define (helper i)
    (if (= i (string-length expr))
        #t
        (if (char-numeric? (string-ref expr i))
            (helper (next i))
            #f
            )
        )
    )
  (if (= 0 (string-length expr))
      #f
      (helper 0)
      )
  )

(define (min-and-positive-index char1 char2 str)
  (cond
    [(> 0 (index-of char1 (substring str 1))) (index-of char2 (substring str 1))]
    [(> 0 (index-of char2 (substring str 1))) (index-of char1 (substring str 1))]
    [else (min (index-of char2 (substring str 1)) (index-of char1 (substring str 1)))]
    )
  )
(define (empty-tree-str? str)
  (string=? str "*")
  )
(define (get-root str)
  (substring str 1 (1+ (min-and-positive-index #\* #\{ str)))
  )
(define (get-left str)
  (let [(x (substring (substring str 1) (min-and-positive-index #\* #\{ str)))]
    (substring x 0 (find-closing-bracket x))
    )                       
  )
(define (get-right str)
  (let* [(x (substring (substring str 1) (min-and-positive-index #\* #\{ str)))
         (y (substring x (find-closing-bracket x)))]
    (substring (string-trim y) 0 (find-closing-bracket (string-trim y)))
    ) 
  )
(define (tree? str)
  (tree?* (string-trim str))
  )

(define (dropWhile p? lst)
  (cond [(null? lst) '()]
        [(p? (head lst)) (dropWhile p? (tail lst))]
        [else lst]))
(define (takeWhile p? lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (if (p? (head lst))
            (cons (head lst) (takeWhile p? (tail lst)))
            res
            )
        )
    )
  (helper lst '())
  )
  
(define (string-trim str)
  (define (string-reverse str)
    (define (reverse lst res)
      (if (null? lst)
          res
          (reverse (tail lst) (cons (head lst) res))
          )
      )
    (list->string (reverse (string->list str) '()))
    )
  (define (cut str)
    (list->string (dropWhile (lambda (x) (char=? x #\space)) (string->list (string-reverse str))))
    )
  (cut (cut str))
  )
(define (root?* str)
  (number? str)
  )
(define (tree?* str)
  (cond [(string=? str "") #f]
        [(string=? str "*") #t]
        [(and (char=? (string-ref str 0) #\{) (char=? (string-ref str (- (string-length str) 1)) #\}))
         (and
          (root?* (string-trim (get-root str)))
          (tree?* (get-left str))
          (tree?* (get-right str))
          (= (string-length (remove-spaces str))
             (+ (string-length (remove-spaces (get-root str)))
                (string-length (remove-spaces (get-left str)))
                (string-length (remove-spaces (get-right str)))
                2))
          )]
        [else #f]
        )
  )
(define (string->tree str)
  (if (tree? str)
      (construct-tree (remove-spaces str))
      #f
      )
  )
(define (str->int str)
  (define (helper i res)
    (if (= i (string-length str))
        res
        (helper (next i) (+ (- (char->integer (string-ref str i)) (char->integer #\0)) (* res 10)))
        )
    )
  (helper 0 0)
  )
(define (construct-tree str)
  (if (empty-tree-str? str)
      empty-tree
      (make-tree (str->int (get-root str)) (construct-tree (get-left str)) (construct-tree (get-right str)))
      )
  )
(define (leaf? tree)
  (cond [(empty-tree? tree) #f]
        [(and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) #t]
        [else #f]
        )
  )

(define (balanced? tree)
  (define (height tree)
    (cond [(or (empty-tree? tree) (leaf? tree)) 0]
          [else (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))]
          )
    )
  (cond [(or (empty-tree? tree) (leaf? tree)) #t]
        [else (and (<= (abs (- (height (left-tree tree)) (height (right-tree tree)))) 1)
                   (balanced? (left-tree tree))
                   (balanced? (right-tree tree)))]
        )
  )

(define (ordered? tree)
  (cond [(or (empty-tree? tree) (leaf? tree)) #t]
        [(and
          (or (empty-tree? (left-tree tree)) (<= (root-tree (left-tree tree)) (root-tree tree)))
          (or (empty-tree? (right-tree tree)) (> (root-tree (right-tree tree)) (root-tree tree)))
          (ordered? (left-tree tree))
          (ordered? (right-tree tree))
          ) #t]
        [else #f]
        )
  )
(define (tree->string tree)
  (if (empty-tree? tree)
      "*"
      (string-append "{" (number->string (root-tree tree)) " " (tree->string (left-tree tree)) " " (tree->string (right-tree tree)) "}")
      )
  )

(define (num-length num)
  (define (helper num res)
    (if (= 0 num)
        res
        (helper (quotient num 10) (+ 1 res))
        )
    )
  (if (= 0 num)
      1
      (helper num 0)
      )
  )
(define (vwidth v-tree)
  (define (lengths lst)
    (map string-length lst)
    )
  (foldr max (head (lengths v-tree)) (tail (lengths v-tree)))
  )
(define (make-indent p? size)
  (if p?
      (string-append "|" (make-string (- size 1) #\space))
      (make-string size #\space)
      )
  )

(define (visualize* lines tree)
  (define (move-right p? n lst)
    (map (lambda (row)
           (string-append (make-indent p? n) row))
         lst)
    )
  (define (add-root left-width root lst)
    (append (list (string-append (number->string root)
                                 (make-string (+ 1 (- left-width (num-length root))) #\-)
                                 (substring (head lst) (+ 1 left-width))))
            (tail lst))
    )
  (cond [(empty-tree? tree) (list "")]
        [(leaf? tree) (list (number->string (root-tree tree)))]
        [else
         (let*
             ([right (visualize* lines (right-tree tree))]
              [left (visualize* lines (left-tree tree))]
              [root (list (number->string (root-tree tree)))]
              [move (move-right #t (+ lines (+ 1 (max (num-length (root-tree tree)) (vwidth left)))) right)])
           (cond [(empty-tree? (right-tree tree)) (append root (list "|") left)]
                 [(empty-tree? (left-tree tree)) (add-root (+ lines (num-length (root-tree tree)))
                                                           (root-tree tree)
                                                           (move-right #f (+ lines (+ 1 (num-length (root-tree tree)))) right))]
                 [(or (leaf? (left-tree tree)) (= (length right) 1))
                  (append (add-root (+ lines (max (num-length (root-tree tree)) (vwidth left)))
                                    (root-tree tree)
                                    move)
                          (list "|")
                          left)
                  ]
                 [else
                  (append (add-root (+ lines (max (num-length (root-tree tree)) (vwidth left)))
                                    (root-tree tree)
                                    move)
                          left)]
                 )
           )
         ]
        )
  )
(define (visualize tree)
  (let ([horizontal 1])
    (foldr (lambda (row rest) (string-append row "\n" rest)) "" (visualize* horizontal tree))
    )
  )