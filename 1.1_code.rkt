#lang racket
(define a 3)
(define b (+ a 1))
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond((= a 4) 6)
     ((= b 4) (+ 6 7 a))
     (else 25))
(+ 2 (if (> b a) b a))
;;1.2 exercises
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2)(- 2 7)))
;;1.3 exercises
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
;;1.4 exercises
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
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)