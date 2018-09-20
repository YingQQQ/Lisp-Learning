#lang sicp
; 用高阶函数做抽象

(define (cube x)
  (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ 1 a) b ))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ 1 a) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;抽象出公共部分

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (sum-fast-cubes a b)
  (sum cube a inc b))

(sum-fast-cubes 1 4)


;exercise 1.29 汤普森规则计算

(define (odd? n)
  (= (remainder n 2) 0))
(define (simpson  f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (cond ((or (= k 0) (= k n))
               1)
          ((odd? k)
           2)
          (else 4)))
  (define (factor k)
    (f (+ a (* h k))))
  
  (define (term k)
    (* (y k)
       (factor k)))
  (define (next k)
    (+ k 1))
       
  (if (not (odd? n))
      (display "请输入偶素")
      (* (/ h 3)
         (sum term (exact->inexact a) next n))))
 ;;(exact->inexact 0) 用于把分数转换为浮点数           
(simpson cube 0 1 1000)
(simpson cube 0 1 100)
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2)) add-dx b)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; exerise 1.30

(define (fast-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result)))) 
  (iter a 0))
  
(define (sum-faster-cubes a b)
  (fast-sum cube a inc b))

(sum-faster-cubes 1 100)

;; exercise 1.31

;递归版本
(define (product term a next b)
  (if (> a b)
      1 ;因为是乘积所以最后一个值不能是0
      (* (term a)
         (product term (next a) next b))))
  
;;lambda匿名函数声明
(product (lambda (x) x)
         1
         (lambda (y) (+ y 1))
         10)
;迭代版本
(define (fast-product term a next b)
  (define (iter a result)
    (if (> a b)
        result ;因为是乘积所以最后一个值不能是0
        (iter (next a) (* (term a) result))))
  (iter a 1))

(fast-product (lambda (x) x)
         1
         (lambda (y) (+ y 1))
         10)
;;重新定义乘阶函数
(define (factorial n)
  (fast-product (lambda (x) x)
         1
         (lambda (y) (+ y 1))
         n))

(define (bottom-iter i)
  (cond ((= i 1)
         3)
        ((odd? i)
         (+ i 1))
        (else (+ i 2))))

(define (top-iter n)
  (cond ((= n 1)
         2)
        ((odd? n)
         (+ n 2))
        (else (+ n 1))))

(define (pi n)
  (* 4
     (exact->inexact (/ (fast-product top-iter
                                      1
                                      (lambda (i) (+ i 1))
                                      n)
                        (fast-product bottom-iter
                                      1
                                      (lambda (i) (+ i 1))
                                      n)))))
  
(pi 1000)

;;exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

(define (resum term a next b)
  (accumulate +
              0
              term
              a
              next
              b))
(define (reproduct term a next b)
  (accumulate *
              1
              term
              a
              next
              b))

;递归版本
(define (fast-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a))))) 
  (iter a null-value))


;; 素数检查 费马小定律
(define (square x)
  (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((odd? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
         
(define (prime-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0)
         true)
        ((prime-test n)
         (fast-prime? n (- times 1)))
        (else false)))
(define (prime? n)
  (fast-prime? n 10))


















