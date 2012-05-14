;;; 2.1.1  Example: Arithmetic Operations for Rational Numbers
;;; code -----------------------------------------------
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
;;; code -----------------------------------------------
;;; Exercise 2.1.
(define (make-rat n d)
  (if (positive? d)
      (cons n d)
      (cons (- n) (- d)))
  )
(print-rat (make-rat 1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat -1 -2))

;;; 2.1.2  Abstraction Barriers
;;; Exercise 2.2.
;;; point constructor
(define (make-point x y) (cons x y))
;;; point selector
(define (x-point point) (car point))
(define (y-point point) (cdr point))
;;; segment constructor
(define (make-segment a b) (cons a b))
;;; segment selector
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (midpoint-segment segment)
  (define (average a b)
    (/ (+ a b) 2))
  (make-segment (average (x-point (start-segment segment)) (x-point (end-segment segment)))
                (average (y-point (start-segment segment)) (y-point (end-segment segment)))))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(print-point (make-point 1 3))
(define (print-segment segment)
  (newline)
  (print-point (start-segment segment))
  (display "-->")
  (print-point (end-segment segment))
  (newline))
(print-segment (make-segment (make-point 1 2) (make-point 3 4)))
(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 3 4))))
;;; Exercise 2.3.
TODO
;;; Exercise 2.4.
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
;;; verify
(car (cons x y))
((cons x y) (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
(x)
(define (cdr z)
  (z (lambda (p q) q)))
;;; Exercise 2.5.
(define (cons a b)
  (* (exp 2 a) (exp 3 b)))
(define (car c)
  (get-exp 2 c))
(define (cdr c)
  (get-exp 3 c))
(define (get-exp base n)
  ;; invariant: base^counter * n is constant
  (define (get-exp-iter base n counter)
    (if (not (= (remainder n base) 0))
        counter
        (get-exp-iter base (/ n base) (+ counter 1))))
  (get-exp-iter base n 0))
(define (exp base n)
  (define (iter base n result)
    (if (= n 0)
        result
        (iter base (- n 1) (* base result))))
  (iter base n 1))
(car (cons 3 2))
(cdr (cons 3 2))

;;; Exercise 2.6.

(zero)
(lambda (f))
(lambda (x))

;;; 2.1.4  Extended Exercise: Interval Arithmetic
;;; Exercise 2.7.
;;; Exercise 2.8.
;;; Exercise 2.9.
;;; Exercise 2.10.
;;; Exercise 2.11.
;;; Exercise 2.12.
;;; Exercise 2.13.
;;; Exercise 2.14.
;;; Exercise 2.15.
;;; Exercise 2.16.

;;; 2.2  Hierarchical Data and the Closure Property
;;; code

;;; code
;;; Exercise 2.17.
(null? (list))
(pair? (list 1 2))
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))
(last-pair (list 23 72 149 34))
;;; Exercise 2.18.
;;; recursive version
(define (reverse list)
  (define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
  (if (null? list)
      list
      (append (reverse (cdr list)) (cons (car list) '()))))
(reverse (list 1))
(append '() (cons 1 '()))

;;; iterative version
(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items '()))
(reverse (list 1 2 3 4 5))
;;; Exercise 2.19.
;; (define (cc amount coin-values)
;;   (cond ((= amount 0) 1)
;;         ((or (< amount 0) (no-more? coin-values)) 0)
;;         (else
;;          (+ (cc amount
;;                 (except-first-denomination coin-values))
;;             (cc (- amount
;;                    (first-denomination coin-values))
;;                 coin-values)))))
(define (cc amount coin-values)
  ;; (display amount)
  ;; (newline)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (except-first-denomination coin-values)
  (if (null? coin-values)
      '()
      (cdr coin-values)))
(define (first-denomination coin-values)
  (if (null? coin-values)
      '()
      (car coin-values)))
(define (no-more? coin-values)
  (null? coin-values))
(no-more? (except-first-denomination (list 1)))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)
(cc 100 uk-coins)


;;; Exercise 2.20.
(define (same-parity x . args)
  (define (same-parity-iter x args)
    (if (null? args)
        (cons x '())
        (if (= (remainder x 2) (remainder (car args) 2))
            (cons x (same-parity-iter (car args) (cdr args)))
            (same-parity-iter x (cdr args)))))
  (same-parity-iter x args))
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;;; Mapping over lists

;;; Exercise 2.21.
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4))

;;; Exercise 2.23.
(define (for-each proc list)
  (cond
   ((null? list) #t)
   (else (proc (car list))
         (for-each proc (cdr list)))))
(define (for-each proc items)
  ;; cond support multiline branching
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;;; 2.2.2  Hierarchical Structures
;;; Exercise 2.24.
(list 1 (list 2 (list 3 4)))
;;; Exercise 2.25.
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 4 (list 5 (list 6 7)))))))))))))))
;;; Exercise 2.26.
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)
;;; Exercise 2.27.

(define (wang-deep-reverse lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append
          (wang-deep-reverse (cdr lst))
          (list (wang-deep-reverse (car lst)))))
        (else
         (append
          (wang-deep-reverse (cdr lst))
          (list (car lst))))))

(define (wang-deep-reverse lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else
         (if (pair? (car lst))
             (append
              (wang-deep-reverse (cdr lst))
              (list (wang-deep-reverse (car lst))))
             (append
              (wang-deep-reverse (cdr lst))
              (wang-deep-reverse (car lst)))))))

(define x (list (list 1 2) (list 3 4)))
(wang-deep-reverse x)

;;; Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

(define (fringe lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append
               (fringe (car lst))
               (fringe (cdr lst))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)

;;; Exercise 2.29.
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;;; b
(define (mobile? structure)
  (pair? structure))
(define (total-weight node)
  ;; branch connects eigther a mobile or a weight
  (if (not (mobile? node))
      node
      (+ (total-weight (left-branch node))
         (total-weight (right-branch node)))))
;;; c
(define (top-left-torque node)
  (define (iter node result)
    (if (not (mobile? node))
        (* result node)
        (iter (branch-structure (left-branch node)) (+ (branch-length (left-branch node)) result))))
  (iter node 0))
(define (top-right-torque node)
  (define (iter node result)
    (if (not (mobile? node))
        (* result node)
        (iter (branch-structure (right-branch node)) (+ (branch-length (right-branch node)) result))))
  (iter node 0))
(define (balanced node)
  (if (not (mobile? node))
      #t
      (and
       (= (top-left-torque node) (top-right-torque node))
       (balanced (branch-structure (left-branch node)))
       (balanced (branch-structure (right-branch node))))))
;;; test
(make-mobile (make-branch 2 3) (make-branch 2 3))
(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(total-weight a)
(top-left-torque a)
(balanced a)
(define b (make-mobile (make-branch 2 3) (make-branch 4 5)))
(total-weight b)
(balanced b)
(define c (make-mobile (make-branch 5 a) (make-branch 3 b))) ;unblanced
(define c (make-mobile (make-branch 5 a) (make-branch 3 (make-mobile (make-branch 2 6) (make-branch 4 3))))) ;balanced
(total-weight c)
(balanced c)

;;; d
left-branch
right-branch
branch-structure
branch-length
;;; Mapping over trees
(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(car tree)
(cdr tree)
(cons (car tree) (cdr tree))
(list 1 (list 2 (list 3 4) 5) (list 6 7))


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(define (square-tree tree)
  (define (square x)
    (* x x))
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (square-tree tree)
  (define (square x)
    (* x x))
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(1 (4 (9 16) 25) (36 49))
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
;;; Exercise 2.31.
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;;; Exercise 2.32.
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set) (append (list (car s)) set)) rest)))))

(subsets (list 1 2 3))
;;; 2.2.3  Sequences as Conventional Interfaces
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
;;; Exercise 2.33.
;;; reference
(define (map p sequence)
  (if (null? sequence)
      '()
      (cons (p (car sequence))
            (map p  (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate (lambda (x y) (+ x y)) 0 (list 1 2 3 4))
(map (lambda (x) (* x x)) (list 1 2 3))
(define (wang-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(wang-map (lambda (x) (* x x)) (list 1 2 3 4))
(define (wang-append seq1 seq2)
  (accumulate cons seq2 seq1 ))
(wang-append (list 1 2 3) (list 4 5 6))
(define (wang-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(wang-length (list 1 2 3))

;;; Exercise 2.34.
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

;;; Exercise 2.35.
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 0
              (map (lambda (sub-tree)
                     (if (not (pair? sub-tree))
                         1
                         (count-leaves sub-tree))) t)))
(count-leaves (list 1
                    (list 2 (list 3 4) 5)
                    (list 6 7)))

;;; Exercise 2.36.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 seqs)

;;; Exercise 2.37.
(define (matrix-*-vector m v)
  (map (lambda (m-line) (accumulate (lambda (x y) (+ x y)) 0 (accumulate-n (lambda (x y) (* x y)) 1 (list m-line v)))) m))
(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list 1 3 5))
(matrix-*-vector m v)
(define (transpose mat)
  (accumulate-n cons '() mat))
(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (line) (map (lambda (col) (accumulate-n (lambda (x y) (* x y)) 1 (list line col))) cols)) m)))

(matrix-*-matrix m m)

;;; Exercise 2.38.
TODO
;;; Exercise 2.39.
TODO
;;; Nested Mappings
(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 4)))

(accumulate append '() (list (list 1 2 3) (list 4 5 6)))

(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(flatmap (lambda (x) (list (list x 100))) (list 1 2 3))
(define (make-pair-sum pair)
  (list (car pair) ()))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 10)

;;; Exercise 2.40.
(define (unique-pairs n)
  (flatmap
   (lambda (i) (map
           (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
   (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;; Exercise 2.41.
(define (triple-sum-equal s)
  (lambda (triple) (= s (accumulate + 0 triple))))

(define (all n s)
  (filter
   (lambda (triple) (= s (accumulate + 0 triple)))
   (flatmap append (flatmap append (map
                                    (lambda (k)
                                      (map (lambda (j) (map (lambda (i) (list i j k)) (enumerate-interval 1 n)))
                                           (enumerate-interval 1 n)))
                                    (enumerate-interval 1 n))))))
(define (all n limit sum)
  (if (= n 0)
      (list '())
      (map (lambda (i) (map (lambda (others) (cons i others)) (all (- n 1) limit (- sum i)))) (enumerate-interval 1 limit))))
(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (all n limit sum)
  (cond ((<= sum 0) (list '()))
        ((= n 1) (list (list sum)))
        (else
         (flatmap (lambda (i) (map (lambda (others) (cons i others)) (all (- n 1) limit (- sum i)))) (enumerate-interval 1 limit)))))
(define (fixed-all n limit sum)
  (filter (lambda (lst) (= (accumulate (lambda (x y) (+ 1 y)) 0 lst) n)) (all n limit sum)))
(fixed-all 2 10 20)

;;; Exercise 2.42.
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define empty-board '())
(define (safe? k positions)
  (define (iter distance positions)
    (if (null? (cdr positions))
        #t
        (and (> (abs (- (abs (- (car positions) (cadr positions))) distance)) 0)
             (> (abs (- (car positions) (cadr positions))) 0)
             (iter (+ 1 distance) (cons (car positions) (cdr (cdr positions)))))))
  (iter 1 positions))
(safe? (list 4 2 8 5 7 1 3 6 ))
(safe? (list 1 2 3))
(queens 7)
(queens 8)
(define (safe? k positions)
  ((let (k-position) (accumulate (lambda (x y) y)))))
(accumulate )
(define (safe? k positions)
  ())
(define (add-position position lst)
  (if (null? (cdr lst))
      (list (list position (car lst)))
      (append (list (list position (car lst))) (add-position (+ position 1) (cdr lst)))))
(map (lambda (tuple) ()))
(define positions (list 4 2 8 5 7 1 3 6))
(map (lambda (tuple) (or (= (car tuple) (cadr tuple)) (= (car tuple) 0)))
     (add-position 1 (map (lambda (x) (- x (car positions))) (cdr positions))))
(map (lambda (tuple) (or (= (car tuple) (cadr tuple)) (= (car tuple) 0)))
     (add-position 1 (map (lambda (x) (- x (car positions))) (cdr positions))))
(accumulate (lambda (x y)
              (+ (if (x)
                     1
                     0) y)) 0 (list #t #t #f))


(cdr (list 1 2))
(+ 1 (accumulate (lambda (x y) (+ 1 y)) 0 lst))
(accumulate (lambda (x y) (+ 1 y)) 0 lst)
(accumulate (lambda (x y) (cons (- (car y) 1) y)) (list (+ 1 (accumulate (lambda (x y) (+ 1 y)) 0 lst))) lst)
(accumulate (lambda (x y) (cons (- (car y) 1) y)) (list 5) lst)

(!= 1 0)


;;; Exercise 2.43.
TODO

(cons 1 (list 2 3 4))
(cons (list 1 2 3) 4)
(list (list 1 2 3 4) 5)
(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))
(my-append (list 1 2 3) (list 4 5 6))
;;;
;;;
;;; 2.3  Symbolic Data
;;; Exercise 2.53.

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(cadr '(x1 x2))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue shoes)))
(memq 'red '(red shoes blue shoes))
;;; Exercise 2.54.
(define (equal? list1 list2)
  (if (or (not (pair? list1)) (not (pair? list2)))
      (eq? list1 list2)
      (and (equal? (car list1) (car list2))
           (equal? (cdr list1) (cdr list2)))))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;;; Exercise 2.55.
(car ''abc)
(cdr ''abc)

