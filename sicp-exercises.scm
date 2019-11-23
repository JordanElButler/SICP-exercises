(define nil '())

(define (exp b e)
  (if (= e 0) 1
    (* b (exp b (- e 1)))))

; rationals
(define (gcd a b)
  (if (= 0 b) a
    (gcd b (mod a b))))

(define (make-rat num den)
  (if (< den 0) 
    (let ((dd (- den))
        (nn (- num)))
        (let ((div (gcd nn dd)))
          (cons (/ nn div) (/ dd div))))
    (let ((div (gcd num den)))
    (cons (/ num div) (/ den div)))))

(define (numer frac)
  (car frac))

(define (denom frac)
  (cdr frac))

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b)) (* (numer b) (denom a))) (* (denom a) (denom b))))

(define (negate-rat a)
  (make-rat (- (numer a)) (denom a)))

(define (sub-rat a b)
  (add-rat a (negate-rat b)))

(define (mult-rat a b)
  (make-rat (* (numer a) (numer b)) (* (denom a) (denom b))))

(define (invert-rat a)
  (make-rat (denom a) (numer a)))

(define (div-rat a b)
  (mult-rat a (invert-rat b)))
  
(define (equal-rat? a b)
  (= (* (numer a) (denom b)) (* (numer b) (denom a))))

(define (print-rat a)
  (newline)
  (display (numer a))
  (display "/")
  (display (denom a)))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mult-point p s)
  (make-point (* s (x-point p)) (* s (y-point p))))

(define (add-points p q)
  (make-point (+ (x-point p) (x-point q)) (+ (y-point p) (y-point q))))

;;;; found on richard d james playlist
(define (c-curve p1 p2)
  (let ((p3 (midpoint-segment (make-segment p1 p2))))
    (make-segment p1
      (make-point (+ (x-point p3) (- (y-point p3) (y-point p2)))
      (+ (y-point p3) (- (x-point p2) (x-point p3)))))))
;~

(define (repeat f n initial)
  (if (= n 0) initial
    (repeat f (- n 1) (f initial))))

(define (square x) (* x x))

(define (repeat-c-curve n segment)
  (define (f segment) (c-curve (start-segment segment) (end-segment segment)))
  (repeat f n segment))

(define (make-segment p q)
  (cons p q))

(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (midpoint-segment l)
  (mult-point (add-points (start-segment l) (end-segment l)) 0.5))

(define (rect-perimeter rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))

(define (rect-area rect)
  (* (width-rect rect) (height-rect rect)))

(define (make-rect c w h)
  (let ((get-width (lambda () w))
      (get-height (lambda () h))
      (get-center (lambda () c)))
    (cons get-center (cons get-width get-height))))

(define (width-rect rect)
  ((car (cdr rect))))

(define (height-rect rect)
  ((cdr (cdr rect))))


(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; interval arithmetic exercise

;;; first level
(define (make-interval lower upper)
  (cons lower upper))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mult-interval x y)
  (let ((p1 (* lower-bound x) (lower-bound y))
        (p2 (* lower-bound x) (upper-bound y))
        (p3 (* upper-bound x) (lower-bound y))
        (p4 (* upper-bound x) (upper-bound y)))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mult-interval x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (let ((p1 (- (upper-bound x) (upper-bound y)))
        (p2 (- (upper-bound x) (lower-bound y)))
        (p3 (- (lower-bound x) (upper-bound y)))
        (p4 (- (lower-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (width-interval int)
  (* 0.5 (- (upper-bound int) (lower-bound int))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (uppder-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (* 0.01 p))))

(define (list-ref l n)
  (if (= n 0) (car l)
    (list-ref (cdr l) (- n 1))))

(define (list-length l)
  (if (null? l) 0
    (+ 1 (list-length (cdr l)))))

(define (list-length-iter l)
  (define (inner ll c)
    (if (null? ll) c
      (inner (cdr ll) (+ c 1))))
  (inner l 0))

(define (list-append m n)
  (if (null? m) n
    (cons (car m) (list-append (cdr m) n))))

(define (last-pair l)
  (if (null? (cdr l)) l
    (last-pair (cdr l))))

(define (reverse-list l)
  (define (inner-reverse ll cc)
    (if (null? ll) cc
      (inner-reverse (cdr ll) (cons (car ll) cc))))
  (inner-reverse l '()))

(define (change-coins amount denominations)
  (define (inner-cc am den)
    (cond ((= 0 (list-length den)) 0)
          ((= am 0) 1)
          ((< am 0) 0)
          (else (+ (inner-cc (- am (car den)) den)
                    (inner-cc am (cdr den))))))
  (inner-cc amount denominations))

(define (list-filter l pred)
  (cond ((null? l) l)
        ((pred (car l)) (cons (car l) (list-filter (cdr l) pred)))
        (else (list-filter (cdr l) pred))))

(define (same-parity g . x)
  (cons g (list-filter x (lambda (y) (= 0 (mod (- y g) 2))))))

(define (m-map proc items)
  (if (null? items) nil
    (cons (proc (car items))
      (map (proc (cdr items))))))

(define (square x) (* x x))

; trees

(define (count-leaves l)
  (cond ((null? l) 0)
    ((not (pair? l)) 1)
    (else (+ (count-leaves (car l)) (count-leaves (cdr l))))))

(define (id x) x)

(define (deep-reverse items)
  (define (inner-deep ll cc)
    (cond ((null? ll) cc)
          ((pair? (car ll)) (inner-deep (cdr ll) (cons (deep-reverse (car ll)) cc)))
          (else (inner-deep (cdr ll) (cons (car ll) cc)))))
  (inner-deep items '()))

(define (fringe items)
  (cond ((null? items) '())
        ((pair? (car items)) (append (fringe (car items)) (fringe (cdr items))))
        (else (cons (car items) (fringe (cdr items))))))


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

(define (is-mobile? structure)
  (pair? structure))

(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((bs (branch-structure branch)))
      (if (is-mobile? bs) (total-weight bs) bs)))
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (+ (branch-weight lb) (branch-weight rb))))

(define (balanced-mobile? mobile)
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (let ((ls (branch-structure lb))
          (rs (branch-structure rb)))
      (let ((lw (if (is-mobile? ls) (total-weight ls) ls))
            (rw (if (is-mobile? rs) (total-weight rs) rs)))
        (and (= (* (branch-length lb) lw) (* (branch-length rb) rw)) (if (is-mobile? ls) (balanced-mobile? ls) #t) (if (is-mobile? rs) (balanced-mobile? ls) #t))))))


(define (tree-map f tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree)) (tree-map f (cdr tree))))))

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; signal flows: map, filter, accumulate, enumerate

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
            (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
       '()
       (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
  (accumulate
    +
    0
    (map square
      (filter (lambda (x) (= 1 (mod x 2))) 
        (enumerate-tree tree)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fibs n)
  (accumulate
    cons
    '()
    (filter (lambda (x) (= 0 (mod x 2)))
      (map fib
        (enumerate-interval 0 n)))))

(define (s-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (s-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (s-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (acc-count-leaves t)
  (accumulate 
    (lambda (x y) 
      (+ (s-length x) y))
    0 
    (map 
      (lambda (x) 
        (cond ((null? x) 0)
              ((not (pair? x)) 1)
              (else (enumerate-tree x))))
      t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) nil mat))

;!
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (accumulate-n (lambda (x y) (cons (dot-product row x) y)) nil cols)) m)))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fr-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (fl-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (ordered-pairs n)
  (accumulate 
    append
    nil
    (map (lambda (i) (map (lambda (x) (list x i)) (enumerate-interval 1 i))) (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (prime? n)
  (= 0 (length (filter (lambda (y) (= 0 y)) (map (lambda (x) (mod n x)) (enumerate-interval 2 (- n 1)))))))

(define (prime-debug? n)
  (map (lambda (x) (mod n x)) (enumerate-interval 2 (- n 1))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum?
      (flatmap
        (lambda (i)
          (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s) (list nil)
    (flatmap (lambda (x) (map (lambda (p) (cons x p))
                                          (permutations (remove x s))))
              s)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (unique-trips n)
  (flatmap 
    (lambda (i) (flatmap 
                  (lambda (j) (map (lambda (k) 
                                (list i j k))
                                (enumerate-interval 1 (- j 1)))) 
                  (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (unique-ordered-trip n s)
  (define (sum-pred? t)
    (let ((i (car t))
          (j (cadr t))
          (k (caddr t)))
      (= s (+ i j k))))
  (flatmap permutations (filter sum-pred? (unique-trips n))))

; queens

(define empty-board nil)

(define (adjoin-position row k rest)
  (cons (list row k) rest))

(define (safe? k positions)
  (define (intersection? p q)
    (let ((rd (- (car p) (car q)))
          (cd (- (cadr p) (cadr q))))
        (or (= rd cd) (= (car p) (car q)) (= (cadr p) (cadr q)))))
  (define (inner m n)
    (cond ((null? n) #t)
        (else (and (not (intersection? m (car n))) (inner m (cdr n))))))
  (let ((m (car (filter (lambda (x) (= k (cadr x))) positions))))
    (let ((n (remove m positions)))
      (inner m n))))

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

; painters and frames

(define (make-vect x y)
  (list x y))

(define (x-cor-vect v)
  (car v))

(define (y-cor-vect v)
  (cadr v))

(define (add-vect v w)
  (make-vect (+ (x-cor v) (x-cor w)) (+ (y-cor v) (y-cor w))))

(define (scale-vect s v)
  (make-vect (* s (x-cor-vect v)) (* s (y-cor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (x-cor-vect v)
                            (edge1-frame frame))
                (scale-vect (y-cor-vect v)
                            (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define outline-painter
  (let ((tl (make-vect 0 0))
        (tr (make-vect 1 0))
        (bl (make-vect 0 1))
        (br (make-vect 1 1)))
      (segments->painter (list (make-segment tl tr) (make-segment tr br) (make-segment br bl) (make-segment bl tl)))))

(define x-painter
  (let ((tl (make-vect 0 0))
        (tr (make-vect 1 0))
        (bl (make-vect 0 1))
        (br (make-vect 1 1)))
      (segments->painter (list (make-segment tl br) (make-segment tr bl)))))

(define diamond-painter
  (let ((tm (make-vect 0.5 0))
        (bm (make-vect 0.5 1))
        (lm (make-vect 0 0.5))
        (rm (make-vect 1 0.5)))
      (segments->painter (list (make-segment tm rm) (make-segment rm bm) (make-segment bm lm) (make-segment lm tm)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                      (make-vect 0.5 0.5)
                      (make-vect 1.0 0.5)
                      (make-vect 0.5 1)))

(define (rotate90 painter)
  (transform-painter painter
                      (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                      (make-vect 0.0 0.0)
                      (make-vect 0.65 0.35)
                      (make-vect 0.35 0.65)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
            (transform-painter painter1
                                split-point
                                (make-vect 0.5 0.5)
                                (make-vect 0.0 1.0)))
          (paint-bottom
            (transform-painter painter2
                                (make-vect 0.0 1.0)
                                (make-vect 1.0 1.0)
                                split-point)))
        (lambda (frame)
          (paint-top frame)
          (paint-bottom frame)))))

; quotes

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
            (deriv (base exp) var)))
        (else 
          (error "unknown expression type -- DERIV" exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


;; modify sums and products to represent arbitrary number of sums and products
; addend is first term and augend is sum of rest of terms
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) b)
        ((and (number? b) (number? exponent) (exp b exponent)))
        (else (list '** b exponent))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

; sets : union-set intersection-set element-of-set? adjoin-set

(define true #t)
(define false #f)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (adjoin-set (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else
          (adjoin-set (car set1) (union-set (cdr set1) set2)))))

; ordered representation

(define (o-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set) false)
        (else (o-element-of-set? x (cdr set))))))

(define (o-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (o-intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (o-intersection-set (cdr set1) set2))
            (else (o-intersection-set set1 (cdr set2)))))))

; binary tree representation

(define (set-make-tree entry left right)
  (list entry left right))

(define (set-entry tree)
  (car tree))

(define (set-left-branch tree)
  (cadr tree))

(define (set-right-branch tree)
  (caddr tree))

(define (tree-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (set-entry set)) true)
        ((< x (set-entry set)) (tree-element-of-set? x (set-left-branch tree)))
        (else (tree-element-of-set? x (set-right-branch tree)))))

(define (tree-adjoin-element x set)
  (cond ((null? set) (set-make-tree x '() '()))
        ((= x (set-entry set)) set)
        ((< x (set-entry set))
          (set-make-tree 
            (set-entry set)
            (tree-adjoin-element x (set-left-branch set))
            (set-right-branch set)))
        ((> x (set-entry set))
          (set-make-tree
            (set-entry set)
            (set-left-branch set)
            (tree-adjoin-element x (set-right-branch set))))))

(define (list->tree l)
  (partial-list l (length l)))

(define (partial-tree l n)
  (cond ((= n 0) '())
        ((= n 1) (set-make-tree (car l) '() '()))
        (else
          (let ((mid (/ n 2)))
            (let ((mid-entry (get-element l mid)))
              (set-make-tree mid-entry (partial-tree l (- mid 1)) (partial-tree (sub-list l (+ mid 1) n))))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (set-left-branch tree))
              (cons (set-entry tree)
                    (tree->list-1 (set-right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (set-left-branch tree)
                      (cons (set-entry tree)
                            (copy-to-list (set-right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define t1 (set-make-tree 7 (set-make-tree 3 (set-make-tree 1 '() '()) (set-make-tree 5 '() '())) (set-make-tree 9 '() (set-make-tree 11 '() '()))))
(define t2 (set-make-tree 3 (set-make-tree 1 '() '()) (set-make-tree 7 (set-make-tree 5 '() '()) (set-make-tree 9 '() (set-make-tree 11 '() '())))))
(define t3 (set-make-tree 5 (set-make-tree 3 (set-make-tree 1 '() '()) '()) (set-make-tree 9 (set-make-tree 7 '() '()) (set-make-tree 11 '() '()))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (floor (/ (- n 1) 2))))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (set-make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; huffman encoding trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (get-branch bits branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-next-branch (car bits) branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (get-branch (cdr bits) tree))
          (get-branch (cdr bits) next-branch)))))
  (get-branch bits tree))

(define (choose-next-branch bit tree)
  (cond ((= 0 bit) (left-branch tree))
        ((= 1 bit) (right-branch tree))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))

(define (contains? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (contains? x (cdr set)))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))

(define (encode-symbol sym tree)
  (if (null? sym)
      '()
      (navigate-tree sym tree)))

(define (navigate-tree sym branch)
  (if (leaf? branch)
    '()
    (let ((next-bit
      (if (contains? sym (symbols (left-branch branch)))
        0
        1)))
      (let ((next-branch
        (if (= 0 next-bit)
          (left-branch branch)
          (right-branch branch))))
        (cons next-bit (navigate-tree sym next-branch))))))

(define (encode message huffman-tree)
  (flatmap (lambda (s) (encode-symbol s huffman-tree)) message))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge tree-set)
  (cond ((null? tree-set) '())
        ((= 1 (length tree-set)) (car tree-set))
        (else (let ((smallest (car tree-set))
                    (second-smallest (cadr tree-set)))
          (successive-merge (adjoin-set (make-code-tree smallest second-smallest) (cddr tree-set)))))))

(define pairs '((A 4) (B 2) (C 1) (D 7) (E 1)))

(define rock '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

; complex numbers
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
                    
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
    (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                        (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "No known datatype -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "No known dataype -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "No known datatype -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "No known datatype -- ANGLE" z))))
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; used for creating generic operators tagged 
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum) (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum) (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
            (apply proc (map contents args))
            (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

; using tags can in general can create a table and populate it with items indexed by tag and op and retrieve item with tag and op, or use message passing style with operators in some internally stored structure

; polynomial representation: terms and variable, =zero-term, make-poly, largest-term, rest-of-terms, order-term, add-poly, mult-poly, gcd-poly, list of term and order, adjoin-term, make-term coeff order variable


; set!

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
              balance)
      "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
            balance))
  (define (dispatch op)
    (cond ((eq? op 'withdraw) withdraw)
          ((eq? op 'deposit) deposit)
          (else (error "Unknown op -- MAKE-ACCOUNT" op))))
  dispatch)

(define (make-accumulator initial)
  (lambda (sum)
    (begin (set! initial (+ sum initial))
      initial)))

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (if (eq? x 'how-many-calls) count
        (begin (set! count (+ 1 count))
          (f x))))))

(define (make-password-account balance password)
  (let ((account (make-account balance))
        (incorrect-count 0))
    (define (call-the-cops)
      "Cops called")
    (define (check-password pass op)
      (if (eq? pass password)
        (begin 
          (set! incorrect-count 0)
          (account op))
        (begin 
          (set! incorrect-count (+ incorrect-count 1))
          (if (> incorrect-count 2)
            (lambda (x) (call-the-cops))
            (lambda (x) "Incorrect password")))))
    check-password))

(define (make-joint account pass new-pass)
  (define (access p op)
    (if (eq? p new-pass)
        (account pass op)
        (account pass op)))
  (if (equal? "Incorrect password" ((account pass 'withdraw) 0))
      (print "Wrong password to this account")
      access
      ))

(define (last-pair x)
  (if (null? (cdr x)) x
    (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x) (set-car! (car x) 'wow) x)

; wrong!
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
      (count-pairs (cdr x))
      1)))

(define p3 (list 'a 'b 'c))
(define el (list 'a))
(define p4 (cons 'a (cons el el)))
(define el1 (cons 'a '()))
(define el2 (cons el1 el1))
(define p7 (cons el2 el2))


(define (correct-count-pairs l)
  (define (visited p aux)
    (cond ((null? aux) #f)
          ((and (eq? (car p) (caar aux)) (eq? (cdr p) (cdar aux))) #t)
          (else (visited p (cdr aux)))))
; inner counts and updates all non-visited pairs of curr, returning aux and num pairs
  (define (inner aux curr sum)
    (cond ((or (not (pair? curr)) (visited curr aux)) (cons aux sum))
          (else 
            (let ((new-aux (cons curr aux)))
              (let ((car-data (inner new-aux (car curr) 0)))
                (let ((cdr-data (inner (car car-data) (cdr curr) 0)))
                  (cons (car cdr-data) (+ 1 (cdr car-data) (cdr cdr-data)))))))))
  (inner '() l 0))

(define (last-pair l)
  (if (not (pair? (cdr l)))
    l
    (last-pair (cdr l))))

(define (create-cycle n)
  (let ((l (enumerate-interval 1 n)))
    (set-cdr! (last-pair l) l)
    l))

(define (cycle? l)
  (define (same-pair? t h)
    (if (and (pair? t) (pair? h) (eq? (car t) (car h)) (eq? (cdr t) (cdr h)))
      #t
      #f))
  (define (get-next p)
    (if (pair? p)
      (cdr p)
      '()))
  (define (inner tortoise hair)
    (cond ((null? hair) #f)
          ((same-pair? tortoise hair) #t)
          (else (inner (get-next tortoise) (get-next (get-next tortoise))))))
  (inner (get-next l) (get-next (get-next l))))
; leetcode
(define water-basin (list 0 1 0 2 1 0 1 3 2 1 2 1))

(define (rain-water l)
  (define (inc x) (+ x 1))
  (define (inner index stack columns sum currheight)
    (cond ((null? columns) sum)
          ((or (null? stack) (< (car columns) currheight))
            (inner (inc index) (cons (cons (car columns) index) stack) (cdr columns) sum (car columns)))
          (else
            (let ((minheight (min (car columns) (caar stack))))
              (let ((width (- (- index (cdar stack)) 1))
                    (height (- minheight currheight)))
                (let ((water (* width height)))
                  (if (< (car columns) (caar stack))
                    (inner (inc index) (cons (cons (car columns) index) stack) (cdr columns) (+ sum water) minheight)
                    (inner index (cdr stack) columns (+ sum water) minheight))))))))
  (inner 0 '() l 0 0))

(define (count-and-say n)
  (define (seq-count l sym)
    (cond ((or (null? l) (not (= (car l) sym))) 0)
          (else
            (+ 1 (seq-count (cdr l) sym)))))
  (define (get-rest l sym)
    (cond ((null? l) '())
          ((not (= (car l) sym)) l)
          (else
            (get-rest (cdr l) sym))))
  (define (process-seq seq)
    (if (null? seq)
      '()
      (cons (seq-count seq (car seq)) (cons (car seq) (process-seq (get-rest seq (car seq)))))))
  (cond ((= n 1) (list 1))
    (else
      (let ((last-seq (count-and-say (- n 1))))
        (process-seq last-seq)))))


(define (permutations l)
  (define (inner remaining)
    (cond ((null? remaining) '())
          (else
            (let ((el (car remaining)))
              (let ((rest (remove el l)))
                (append (map (lambda (x) (cons el x)) (permutations rest)) (inner (cdr remaining))))))))
  (if (null? l)
    (list '())
    (inner l)))

(define (combinations n k)
  (cond ((> k n) 0)
        ((= k 0) 1)
        (else (+ (combinations (- n 1) (- k 1)) (combinations (- n 1) k)))))

(define (permutations n)
  (if (= n 0)
    1
    (* n (permutations (- n 1)))))

(define (derangements n)
  (define (inner sum count)
    (if (= count 0)
      sum
      (inner (+ sum (* (combinations n count) (derangements (- n count)))) (- count 1))))
  (cond ((= n 0) 1)
        ((= n 1) 0)
        (else (- (permutations n) (inner 0 n)))))

; -leetcode