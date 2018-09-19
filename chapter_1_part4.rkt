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


;execrise 1.29 汤普森规则计算

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



