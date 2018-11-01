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
(display-info (accumulate list nil (list 1 2 3)))
(display-info (fold-left list nil (list 1 2 3)))