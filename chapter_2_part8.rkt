#lang sicp
;集合做为排序的表
(define (element-of-set? x set)
  (cond ((null? set) false) ;空表
        ((= x (car set)) true) ;相等存在
        ((< x (car set)) false) ; 小于则不存在
        (else
         (element-of-set? x (cdr set))))) ;迭代身下元素

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x1 set)
  (if (null? set)
      (list x1)
      (let ((x2 (car set)))
        (cond ((= x1 x2)
               set)
              ((< x1 x2)
               (cons x1 set))
              ((< x2 x1)
               (cons x2 (adjoin-set x1 (cdr set))))))))

(adjoin-set 3 (list 1 2 4 5)) 

;exercise-2.62
;求两个集合的并集
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))
            
(union-set '(1 2 3 4 5) (list 1 3 5 7 9))















                
