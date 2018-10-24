#lang sicp
;层次性数据和闭包性质
(define one-through-four (list 1 2 3 4))
one-through-four
(define int-one-through-four
  (cons 1
        (cons 2
              (cons 3
                    (cons 4 nil)))))
int-one-through-four

(car one-through-four);1
(cdr one-through-four); 2 3 4
(car (cdr one-through-four));2
(car (cdr (cdr (cdr one-through-four))));4
(cons 10 one-through-four);10 1 2 3 4

;表操作

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))

(list-ref squares 0)
(list-ref squares 1)
(list-ref squares 3)
;(list-ref squares 8) 超出8的序列范围则报错,因此我们需要知道是不是空表或则表的长度是多少
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items))))) ;递归版本
(define (better-lenght items)
  (define (length result item)
    (if (null? item)
        result
        (length (+ 1 result)
                (cdr item))))
  (length 0 items)) ;迭代版本
(define odds (list 1 3 5 7))
(length odds)
(better-lenght odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append squares odds)


;execrise-2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))

;exercise-2.18














