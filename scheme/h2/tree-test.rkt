#lang racket/base
(require rackunit rackunit/gui racket/include)

(include "tree.rkt")

(define tree:empty "*")
(define tree:leaf "{   5*  *}")
(define tree:valid1 "{1345        {4  *  *} *   }")
(define tree:valid2 "     {  18 * {36 *   *}}")
(define tree:valid3 "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")

(define tree:two-stars "**")
(define tree:empty-str "")
(define tree:wrong-number-of-brackets "{18 * {36 * *} }}")
(define tree:two-numbers-in-root "{5 {22 {2 17 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
(define tree:negative-root "{-4 * *}")
(define tree:decimal-root "{2.5 * *}")
(define visuzlize-t '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))

(define empty-t '())
(define leaf-t '(5 () ()))
(define unbalanced-t '(4 (2 () (1 () (7 () ()))) ()))
(define valid1-t '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
(define ordered-t '(10 (5 (3 () ()) (6 () ())) (15 () (16 () ()))))
(define ordered-t-str "{10 {5 {3 * *} {6 * *}} {15 * {16 * *}}}")
(define (message str p?)
  (if p?
       (string-append "\"" str "\"" " is valid a valid tree")
       (string-append "\"" str "\"" " is not a valid tree")
      )
  )

(test/gui

(test-suite
 "tree?"
 (test-true (message tree:empty #t) (tree? tree:empty))
 (test-true (message tree:leaf #t) (tree? tree:leaf))
 (test-true (message tree:valid1 #t) (tree? tree:valid1))
 (test-true (message tree:valid2 #t) (tree? tree:valid2))
 (test-true (message tree:valid3 #t) (tree? tree:valid3))
 (test-false (message tree:two-stars #f) (tree? tree:two-stars))
 (test-false (message tree:empty-str #f) (tree? tree:empty-str))
 (test-false (message tree:wrong-number-of-brackets #f) (tree? tree:wrong-number-of-brackets))
 (test-false (message tree:two-numbers-in-root #f) (tree? tree:two-numbers-in-root))
 (test-false (message tree:negative-root #f) (tree? tree:negative-root))
 (test-false (message tree:decimal-root #f) (tree? tree:decimal-root))
 )
(test-suite
 "string->tree"
 (test-true "Empty tree" (equal? (string->tree tree:empty) empty-t))
 (test-true "Leaf tree" (equal? (string->tree tree:leaf) leaf-t))
 (test-true "Random tree" (equal? (string->tree tree:valid3) valid1-t))
 (test-false "Invalid tree returns FALSE" (string->tree  tree:two-numbers-in-root))
 )
(test-suite
 "balanced?"
 (test-false "Unbalanced tree returns false" (balanced? unbalanced-t))
 (test-false "Another unbalanced tree returns false" (balanced? visualize-t))
 (test-true "Balanced tree returns true" (balanced? valid1-t))
 (test-true "Empty tree is balanced" (balanced? empty-t))
 (test-true "Leaf tree is balanced" (balanced? leaf-t))
 )
(test-suite
 "ordered?"
 (test-false "Unordered tree returns false" (ordered? valid1-t))
 (test-true "Ordered tree returns true" (ordered? ordered-t))
 (test-true "Empty tree is ordered" (ordered? empty-t))
 (test-true "Tree with one node is ordered" (ordered? leaf-t))
 )
(test-suite
 "tree->string"
 (test-true "Valid conversion of random tree" (string=? ordered-t-str (tree->string ordered-t)))
 (test-true "Valid conversion of empty tree" (string=? tree:empty (tree->string empty-t)))
 )
)