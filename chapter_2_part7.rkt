#lang sicp
;;实例:集合的表示

(define (equal? s1 s2)
  (if (and (pair? s1) (pair? s2))
      (and (equal? (car s1) (car s2))
      (equal? (cdr s1) (cdr s2)))
      (eq? s1 s2)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else
         (element-of-set? x (cdr set)))))

(define (join-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1)
         set2)
        ((null? set2)
         set1)
        (else
         (let ((subset (union-set (cdr set1) set2)))
           (if (element-of-set? (car set1) subset)
               subset
               (cons (car set1) subset))))))
(union-set '(1 2 3) '(3 4 5 6))

;exercise-2.60
;;允许重复的集合
;element-of-set? 只是用来判断元素是不是在集合内和重复不重复没有关联, O(n)

(define (adjoin-set-exercise x set)
  (cons x set)); O(1)

(adjoin-set-exercise 1 (list 2 3 4))

(define (union-set-exercise set1 set2)
  (append set1 set2)) ;O(1)

(union-set-exercise '(1 2 3) '(3 4 5 6))

;intersection-set 还是 O(n2)


















         