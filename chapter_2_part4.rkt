#lang sicp
;序列作为一种约定界面
(define (square x)
  (* x x))

(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-square (car tree))
                 (sum-odd-square (cdr tree))))))
                 
(define (map proc tree)
  (if (null? tree)
      nil
      (cons (proc (car tree))
            (map proc (cdr tree)))))

(define (display-info f)
  (newline)
  (display f))

(display-info (map square (list 1 2 3 4 5)))

(define (filter predicate sequences)
  (cond ((null? sequences) nil)
        ((predicate (car sequences))
         (cons (car sequences)
               (filter predicate (cdr sequences))))
        (else (filter predicate (cdr sequences)))))

(display-info (filter odd? (list 1 2 3 4 5)))


(define (accumulate op initial sequences)
  (if (null? sequences)
      initial
      (op (car sequences)
          (accumulate op initial (cdr sequences)))))

(display-info (accumulate + 0 (list 1 2 3 4 5)))

(display-info (accumulate cons nil (list 1 2 3 4 5)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(display-info (enumerate-interval 2 7))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(display-info (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))


(define (new-sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                          (enumerate-tree tree)))))

(display-info (new-sum-odd-squares (list 1 (list 2 (list 3 4)) 5)))

;exercise-2.33

(define (new-map p sequences)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequences))
(new-map square (list 1 2))

(define (new-append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequences)
  (accumulate (lambda (x y)
                (+ 1 y))
                0 sequences))


;exercise-2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;exercise-2.35

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (null? x);空表就是0
                         0
                         1));不是空表就是元素则是1
                   (enumerate-tree t))));扁平化表结构

(count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7))))

;exercise-2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car
                                     seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)

;exercise-2.37

(define (dot-product v w)
  (accumulate +
              0
              (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (col)
         (dot-product col v))
       m))

(define (transpose m)
  (accumulate-n cons nil m))
              

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (col-of-m)
                 (map (lambda (col-of-cols)
                          (dot-product col-of-m 
                                       col-of-cols))
                      cols))
             m)))

;exercise-2.38

(define (fold-left op initial seqs)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seqs))

(accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(accumulate + 1 (list 1 2 3))
(fold-left + 1 (list 1 2 3))
(accumulate * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
(accumulate - 1 (list 1 2 3))
(fold-left - 1 (list 1 2 3))
;符合结合律的运算都是有相同的结果


;exercise-2.39

(define (reverse list)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter list nil))

(define (new-reverse-1 seqs)
  (accumulate (lambda (x y)
                (append y (list x)))
              nil
              seqs))
(define (new-reverse-2 seqs)
  (fold-left (lambda (x y)
              (cons y x))
              nil
              seqs))
                
(display-info (new-reverse-1 (list 1 2 3 4)))
                
(display-info (new-reverse-2 (list 1 2 3 4)))


;嵌套映射
(define (even? n)
  (= (remainder n 2) 0))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0)
            true)
          ((fermat-test n)
            (fast-prime? n (- times 1)))
          (else
            false)))

(define (expmod base exp m)
    (cond ((= exp 0)
            1)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                       m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))
(define (prime? n)
    (fast-prime? n 10))
;抽象append过程
(define (flatmap proc seq)
  (accumulate append
              nil
              (map proc seq)))
;最后的过滤条件是 i + j 是否素数,定义谓词：
(define (prime-sum? pair)
  (prime? (+ (car pair)
             (cadr pair))))

;生成三元组
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                   (map (lambda (j)
                          (list i j))
                        (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)
      (list s)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations (remove x s))))
                 s)))

(define (remove item seqs)
  (filter (lambda (x)
            (not (= x item)))
          seqs))

;exercise-2.40

(define (prime-sum-pairs-1 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (unique-pairs n)
    (flatmap (lambda (i)
                 (map (lambda (j) (list i j))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))


(display-info (prime-sum-pairs-1 6))


;exercise-2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n))) ;三元数组

;(display-info (unique-triples 5))

(define (sum-triples lst)
  (if (null? lst)
      0
      (+ (car lst)
         (sum-triples (cdr lst)))))

(define (sum-triples-equal-n sum triples)
  (= sum (sum-triples triples))) ;过滤条件

(define (equal-sum-triples n)
       (filter (lambda (x)
                 (sum-triples-equal-n n x))
               (unique-triples n)))

(display-info  (equal-sum-triples 13));整数13的和三元数组的和也等于13

;exercise-2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board nil)

(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define (iter-check row-of-new-queens rest-of-queens i)
    (if (null? rest-of-queens) ;下方所有皇后检查完毕，新皇后安全
        #t
        (let ((row-of-current-queens (car rest-of-queens)))
          (if (or (= row-of-new-queens row-of-current-queens) ;行比较不能在一行
                  (= row-of-new-queens (+ i row-of-current-queens)) ;不能在对角线
                  (= row-of-new-queens (- row-of-current-queens i))) ;同上
              #f
              (iter-check row-of-new-queens
                          (cdr rest-of-queens); 继续检查剩余的皇后
                          (+ i 1)))))) ;继续比较下一行
    (iter-check (car positions)
              (cdr positions)
              1))

;exercise-2.43

;练习2.42中的queens函数对于每个棋盘(queen-cols k),使用 enumerate-interval 产生 board-size 个棋盘。
;而 Louis 的 queens 函数对于 (enumerate-interval 1 board-size) 的每个 k ，都要产生 (queen-cols (- k 1)) 个棋盘。
;因此， Louis 的 queens 函数的运行速度大约是原来 queens 函数的 board-size 倍，也即是 T * board-size














