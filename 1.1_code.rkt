#lang sicp
;;第一章1.1程序设计的基本元素
(define a 3)
(define b (+ a 1))
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond((= a 4) 6)
     ((= b 4) (+ 6 7 a))
     (else 25))
(+ 2 (if (> b a) b a))
;;1.1.2 exercises
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2)(- 2 7)))
;;1.1.3 exercises
(define (square-plus x y)
  (+ (* x x)(* y y)))
(square-plus 1 2)
(define (biggest x y z)
  (cond ((and (< x y)(< x z))
         ( square-plus z y))
        ((and (< y x)(< y z))
         ( square-plus x z))
        ((and (< z x)(< z y))
         (square-plus x y))
        ))
(biggest 0 2 2)
(biggest 1 2 3)
(biggest 3 5 7)
;;1.1.4 exercises
;;如果是应用序的话,在运行代码的时候由于(define (p)(p))无线循环赋值而报错,如果是正则序的话,因为代码在
;;在调用时候才会求值,而(test 0 (p))的时候代码并不会进入(define (p)(p)的函数,因此不会报错
(define (square x)
  (* x x))
(square 10)

;;1.17/page-14
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
;;#f死假值
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(good-enough? 1.0 9);;#f

(abs (- (square 1.0) 9));;8

(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)

(define (new-good-enough? new-value old-value)
  (> 0.01 (/ (abs (- new-value old-value)) old-value)))
(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess (improve guess x))
      (improve guess x)
      (new-sqrt-iter (improve guess x)
                 x)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))
(new-sqrt 900000000)

;;1.1.8
(define (cube-iter guess x)
  (if (new-good-enough? guess (cube-improve guess x))
      (cube-improve guess x)
      (cube-iter (cube-improve guess x)
                 x)))
(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))
(cube-iter 1.0 (* 100 100 100))

(define (chunk-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(chunk-sqrt 81)

;;第一章1.2过程与它们所产生的计算
(define (new-fact-iter product count)
  (define (new-fact product count min-count)
    (if (< count min-count)
      product
      (new-fact (* product count)
                     (- count 1)
                      min-count)))
  (new-fact product count 1))
(new-fact-iter 6 5)
;; 斐波那契数
(define (old-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (old-fib (- n 1)) (old-fib (- n 2))))))
(old-fib 6)
        
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))
    )
    (fib-iter 1 0 n)
  )
(fib 1)

;; 换零钱的实例
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(count-change 100)
(count-change 11)

;; 树形递归练习题
;;1.11
(define (func n)
  (if (< n 3)
      n
      (+ (* 1 (func (- n 1))) (* 2 (func (- n 2))) (* 3 (func (- n 3))))
            ))
(func 5)
(func 4)
(define (easy-func a b c i n)
  (if (= n i)
      c
      (easy-func (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (+ i 1)
                 n)
      ))
(define (f n)
  (easy-func 2 1 0 0 n))
(f 4)
(define (easy-funcs a b c n)
  (if (= n 0)
      c
      (easy-funcs (+ a (* 2 b) (* 3 c))
                  a
                  b
                  (- n 1))))
(define (fs n)
  (easy-funcs 2 1 0 n))
(fs 5)
;;1.12 题目翻译出错,应该是求三角形各个位置的元素
(define (pascal row col)
  (cond ((> col row)
            (error "unvalid col value"))
        ((or (= col 0) (= row col)) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))
        )
  )
(pascal 4 3)
;;阶乘公式
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter a b n)
  (if (> b n)
      a
      (fact-iter (* a b)
                 (+ b 1)
                 n)))
(define (recursive-pascal row col)
  (/ (factorial row) (* (factorial (- row col)) (factorial col))))
(recursive-pascal 4 3)