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
(define empty (list))
(length empty)
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

(define (reverse list)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter list '()))

(reverse (list 1 4 9 16 25))

(define (reverse-2 items)
  (if (null? items)
      items
      (append (reverse-2 (cdr items))
              (list (car items)))))
(reverse-2 (list 1 4 9 16 25))

;exercise-2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0) (no-more? coin-values))
         0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomaintion coin-values))
                coin-values)))))

(define (no-more-2? coin-values)
  (= 0 (length coin-values)))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomaintion coin-values)
  (car coin-values))

(cc 100 us-coins)

(cc 100 (reverse-2 us-coins))
;结构就是表顺序不同并不会影响结果

;exercise-2.20

(define (same-parity x . y)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (if (= (remainder x 2) (remainder (car items) 2))
                  (append result (list (car items)))
                  result))))
  (iter y (list x)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;对表的映射

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc
                 (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(map (lambda (x)
       (* x x))
     (list 1 2 3 4))

(define (new-scale-list items factor)
  (map (lambda (x)
         (* x factor))
       items))
(new-scale-list (list 1 2 3 4 5) 10)

;exercise-2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (new-square-list items)
  (map (lambda (x)
         (* x x))
       items))

(square-list (list 1 2 3 4))
(new-square-list (list 1 2 3 4))


;exercise-2.22
(define (square x)
  (* x x))

(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
(square-list-1 (list 1 2 3 4)) ;原因是因为第一个索引嵌套的cons层次最多自然在最后面
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                   (square (car things))))))
  (iter items nil))

(square-list-2 (list 1 2 3 4)) ;并不是一个正常的表结构 只是一个有cons嵌套的序对
;实现一个不反转的过程需要借用append函数
(define (square-list-good items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))
(square-list-good (list 1 2 3 4))

;exercise-2.23

(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
       (for-each proc
                (cdr items)))))
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
;2.2.2层次性结构
(newline)
(display (cons (list 1 2) (list 3 4)))
(newline)

(define (count-leaves x)
  (cond ((null? x)
         0)
        ((not (pair? x))
         1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)

;exercise-2.24

(display (list 1 (list 2 (list 3 4))))

;exercise-2.25

(newline)
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(display (list (list 1 2) 3 4))
(newline)
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))















