#lang sicp
;集合为二叉树
(define (entry tree)
  (car tree));顶端数据项

(define (left-branch tree)
  (cadr tree));小于顶端的左边分支

(define (right-branch tree)
  (caddr tree));大于顶端的右边分支

(define (make-tree tree left right)
  (list tree left right))

(define (element-of-set? x set)
  (cond ((null? set)
         '())
        ((= x (entry set))
         true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set)
         (make-tree x '() '()))
        ((= x (entry set))
         set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x
                                (left-branch set));比较左侧小于顶端的分支
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x
                                (right-branch set))))));比较右侧大于顶端的分支


;exercise-2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define a (make-tree 1 nil nil))
(define b (make-tree 5 nil nil))
(define e (make-tree 11 nil nil))
(define f (make-tree 9 nil e))
(define c (make-tree 3 a b))
(define trees (make-tree 7 c f))
(tree->list-1 trees)
(tree->list-2 trees)
;a)
;两个树产生的结果是一样的
;b)
;不是一样的增长速度,tree->list-1是递归,而tree->list->2是迭代.因此后面的增长速度慢一些


;exercise-2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)));quotient 实现N1除以N2,结果值为商
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remianing-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remianing-elts))))))))
                

#|
(1 3)(5 7 9 11)             ; 分割左右子树

(5 7 9 11)                  ; 创建 1 节点
    /
   /
1(3)

   (5 7 9 11)               ; 创建 1 的左子树(空)
      /
     /
   1(3)
   /
  /
'()

    (5 7 9 11)              ; 创建 1 的右子树（包含 3）
      /
     /
    1
   / \
  /   \
'()    3

       5 (7 9 11)           ; 创建树根 5
      /
     /
    1
   / \
  /   \
'()    3

       5                    ; 创建 9 节点
      / \
     /   \
    1     9 (7 11)
   / \
  /   \
'()    3

         5                  ; 创建 9 的左子树（包含 7）
        /\
       /  \
      /    \
     /      \
    1        9 (11)
   / \      /
  /   \    /
'()    3  7

         5                  ; 创建 9 的右子树（包含 11）
        / \
       /   \
      /     \
     /       \
    1         9
   / \       / \
  /   \     /   \
'()    3   7    11
|#

;b)
;对于列表中的每个节点， list->tree 都要执行一次 make-tree （复杂度为 Θ(1) ），将这个节点和它的左右子树组合起来，因此对于长度为 n 的列表来说， list->tree 的复杂度为 对于列表中的每个节点, list->tree 都要执行一次 make-tree (复杂度为 Θ(1) ),将这个节点和它的左右子树组合起来,因此对于长度为 n 的列表来说, list->tree 的复杂度为 Θ(n) 。

;exercise-2.65

(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set-ol (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set-ol (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set-ol set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (list->tree
   (union-set-ol
    (tree->list-2 set1)
    (tree->list-2 set2))))

(define one (list->tree '(1 2 3 4 5)))
(define two (list->tree '(1 3 5 7 9)))
(union-set one two)
(tree->list-2 (union-set one two))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ol (cdr set1)
                                             (cdr set2))))
              ((< x1 x2)
               (intersection-set-ol (cdr set1) set2))
              ((> x1 x2)
               (intersection-set-ol set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (list->tree
   (intersection-set-ol
    (tree->list-2 set1)
    (tree->list-2 set2))))

(intersection-set one two)
(tree->list-2 (intersection-set one two))

;;集合和信息检索

(define (equal? s1 s2)
  (if (and (pair? s1) (pair? s2))
      (and (equal? (car s1) (car s2))
      (equal? (cdr s1) (cdr s2)))
      (eq? s1 s2)))

(define (key records)
  (car records))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records)
         false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup-better given-key records)
  (if (null? records)
      false
      (let ((entry (key (car records))))
        (cond ((= given-key key)
               (car records))
              ((< given-key key)
               (lookup-better given-key
                              (left-branch records)))
               ((> given-key key)
               (lookup-better given-key
                              (right-branch records)))))))
#|
                  (7 "John")
                  /        \
                 /          \
          (3 "Mary")       (19 "Tom")
          /     \
(1 "Peter")    (5 "Jack")
|#












































  
