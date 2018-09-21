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


;; 素数检查 费马小定律 exercise 1.33
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

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor 2))

(define (slow-prime? n)
  (= n (smallest-divisor n)))
                              

;; 迭代版本
(define (fast-filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
    (if (> a b)
        result
        (if (predicate a)
             (iter (next a) (combiner result (term a)))
             (iter (next a) result))))
  (iter a null-value))

;; 递归版本
(define (filtered-accumulate combiner null-value term a next b predicate)
    (if (> a b)
        null-value
        (if (predicate a)
              (combiner (term a)
                (filtered-accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b
                            predicate))
            (filtered-accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b
                            predicate))))

(define (fast-primes-sum a b)
  (fast-filtered-accumulate +
                       0
                       (lambda (x) x)
                       a
                       (lambda (x) (+ x 1))
                       b
                       slow-prime?))

(define (primes-sum a b)
  (filtered-accumulate +
                       0
                       (lambda (x) x)
                       a
                       (lambda (x) (+ x 1))
                       b
                       slow-prime?))


(fast-primes-sum 1 10)
(primes-sum 1 10)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;互素函数
(define (coprime? i n)
  (and (< i n)
       (= 1 (gcd i n))))

(coprime? 2 7)
(coprime? 2 3)
(coprime? 2 4)

(define (product-coprime i n)
  (fast-filtered-accumulate *
                       1
                       (lambda (x) x)
                       i ;; 从1开始到n
                       (lambda (x) (+ x 1))
                       n
                       (lambda (x) (coprime? x n))))

(product-coprime 1 10)



;;1.32

((lambda (x y z)
   (+ x y (square z)))
 1 2 3)


(define (fe x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(fe 1 2)

(define (easy-f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
         (+ (* x (square a))
        (* y b)
        (* a b))))

(easy-f 1 2)

;; execrise 1.34

;将参数2应用于目标函数
(define (f g)
  (g 2))
; square函数传入2
(f square)
;;lambda函数传入2
(f (lambda (z) (* z (+ z 1))))
#|
(f f)含义是f函数传入2
(f 2)
(2 2)
因此解释权无法识别2含义,因为第一个2不是过程
|#