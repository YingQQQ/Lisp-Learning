#lang sicp
;2.1.3数据意味着什么
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 --- CONS" m))))
  dispatch)
(define (car z)
  (z 0))
(define (cdr z)
  (z 1))

(car (cons 5 3))
(cdr (cons 5 3))

;exercise-2.4

(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q)
       p)))

(define (new-cdr z)
  (z (lambda (p q)
       q)))

(new-car (new-cons 9 8))
(new-cdr (new-cons 9 8))

;exercise-2.5

(define (expt-cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (expt-car z)
  (if (= 0 (remainder z 2))
      (+ 1 (expt-car (/ z 2)))
      0))

(define (expt-cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (expt-cdr (/ z 3)))
      0))
(define x (expt-cons 1 2))

(expt-car x)

;execrise-2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
#|
(add-1 zero)
 (add-1 (lambda (f)
         (lambda (x) x)))
 (lambda (f)
  (lambda (x)
    (f ((lambda (f)
          (lambda (x) x) f) x))))
 (lambda (f)
  (lambda (x)
    (f ((lambda (x) x) x))))
 (lambda (f)
  (lambda (x)
    (f x)))
|#
(define one (lambda (f)
              (lambda (x)
                (f x))))

;two===>(add-1 one)
#|
(add-1 (lambda (f)
         (lambda (x)
           (f x))))
(lambda (f)
  (lambda (x)
    (f (((lambda (f)
           (lambda (x)
             (f x))) f) x))))
(lambda (f)
  (lambda (x)
    (f ((lambda (x)
          (f x)) x))))
(lambda (f)
  (lambda (x)
    (f (f x))))
|#
(define two (lambda (f)
              (lambda (x)
                (f (f x)))))
(define three (lambda (f)
                (lambda (x)
                  (f (f (f x))))))

(define four (lambda (f)
               (lambda (x)
                 (f (f (f (f x)))))))
#|
(plus 3 2)
(plus (lambda (f)
            (lambda (x)
             (f (f (f x)))))
   (lambda (f)
            (lambda (x)
             (f (f x)))))
|#
(define plus
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          (m f (n f x)))))))


#|
(plus one two)
(lambad (1)
        (lambda (n)
          (lambda (f)
            (lambda (x)
              (1 f (n f x)))))) 2)
(lambda (2)
  (lambda (f)
    (lambda (x)
      (1 f (2 f x)))))

(lambda (f)
  (lambda (x)
    (1 f (2 f x)))))

(lambda (f)
  (lambda (x)
    (2 f (1 f x))))
(lambda (f)
  (lambda (x)
    (2 f ((lambda (f)
              (lambda (x)
                (f x))) f x))))

(lambda (f)
  (lambda (x)
    (2 f ((lambda (x)
           (f x)) x))))
(lambda (f)
  (lambda (x)
    (2 f (f x))))
(lambda (f)
  (lambda (x)
    ((lambda (f)
              (lambda (x)
                (f (f x)))) f (f x))))
(lambda (f)
  (lambda (x)
    ((lambda (x)
       (f (f x)))
     (f x))))

(lambda (f)
  (lambda (x)
    (f (f (f x))))) ; === three

|#


;2.14 扩展练习:区间算术

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;2.7
(define (make-interval a b)
  (cons a b))

(define (lower-bound r)
  (car r))

(define (upper-bound r)
  (cdr r))

;exercise-2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;test
(define (display-info r)
  (newline)
  (display "[")
  (display (lower-bound r))
  (display ",")
  (display (upper-bound r))
  (display "]"))

(define a (make-interval 1 10))
(define b (make-interval 50 100))
(define test1 (sub-interval b a))
(display-info test1)

;exercise-2.9

(define add-interval-test (add-interval b a))
(display-info add-interval-test)

(define (width-interval interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2.0))
(newline)
(width-interval test1) ;29.5
(width-interval add-interval-test);29.5

(define mul-test (mul-interval b a))
(define div-test (div-interval b a))

(width-interval mul-test) ;475.0
(width-interval div-test) ;47.5

;exercise-2.10
;判断除数不为0就行,区间不包括0
(define (check-zero r)
  (and (<= (lower-bound r) 0)
       (>= (upper-bound r) 0)))

(define (better-div-interval x y)
  (if (check-zero y)
      (error "Error: The denominator should not span 0.")
        (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
      
(define c (make-interval -10 10))
(display-info (div-interval c a))

(display-info (better-div-interval c a))

;exercise-2.11

(define (better-mul-interval x y)
  (let ((xl (lower-bound x))
        (xh (upper-bound x))
        (yl (lower-bound y))
        (yh (upper-bound y)))
    (cond ((and (>= xl 0)
                (>= xh 0)
                (>= yl 0)
                (>= yh 0))
           (make-interval (* xl yl) (* xh yh))) ;[+,+]*[+,+]
          ((and (>= xl 0)
                (>= xh 0)
                (<= yl 0)
                (>= yh 0))
           (make-interval (* xh yl) (* xh yh))) ;[+,+]*[-,+]          
          ((and (>= xl 0)
                (>= xh 0)
                (<= yl 0)
                (<= yh 0))
           (make-interval (* xh yl) (* xl yh))) ;[+,+]*[-,-]
          ((and (<= xl 0)
                (>= xh 0)
                (>= yl 0)
                (>= yh 0))
           (make-interval (* xl yh) (* xh yh))) ;[-,+]*[+,+]
          ((and (<= xl 0)
                (>= xh 0)
                (<= yl 0)
                (>= yh 0))
           (make-interval (min (* xl yh) (* xh yl))
                         (max (* xl yl) (* xh yh)))) ;[-,+]*[-,+]
          ((and (<= xl 0)
                (>= xh 0)
                (<= yl 0)
                (<= yh 0))
           (make-interval (* xh yl) (* xl yl))) ;[-,+]*[-,-]
          ((and (<= xl 0)
                (<= xh 0)
                (>= yl 0)
                (>= yh 0))
           (make-interval (* xl yh) (* xh yl))) ;[-,-]*[+,+]
          ((and (<= xl 0)
                (<= xh 0)
                (<= yl 0)
                (>= yh 0))
           (make-interval (* xl yh) (* xl yl))) ;[-,-]*[-,+]
          (else
           (make-interval (* xh yh) (* xl yl)))))) ;[-,-]*[-,-]

(define d (make-interval -4 -2))
(display-info (better-mul-interval a c))
(display-info (better-mul-interval d d))


;exercise-2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

;center i 不能为0
(define (percent i)
  (* 100 (/ (width i) (center i))))

(define e (make-center-percent 5 20))
(display-info e)
(newline)
(center e)
(width  e)
(percent e)

;exercise-2.13
#|
根据题意可以假设区间A和B,误差分别为P1和P2,中线点为C1和C2,而且所有的数值都是正的
那么区间A=[C1-C1P1, C1+C1P1], B=[C2-C2P2, C2+C2P2]
两个区间相乘 C = AB =[(C1-C1P1)(C2-C2P2), (C1+C1P1)(C2+C2P2)]
                  =[C1C2(1-(P1+P2)+P1P2), C1C2(1+(P1+P2)+P1P2)]
其中因为P1,P2很小则P1P2近似为0.
                  =[C1C2(1-(P1+P2)), C1C2(1+(P1+P2))]
|#

(newline)
(define q (make-center-percent 5 2))
(define w (make-center-percent 10 3))
(define z (mul-interval q w))
(newline)
(percent z)


;exercise-2.14
(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define f (make-center-percent 100 5))
(define g (make-center-percent 200 2))
(define result1 (part1 f f))
(define result2 (part2 f f))
(display-info result1)
(display-info result2)
;result1 不等于 result2. lem正确
(newline)
(define result3 (center (part1 f g)))
(define result4 (center (part2 f g)))
(display result3)
(newline)
(display result4)
;result3 不等于 result4. lem正确
(define ff (div-interval f f)) ;A/A
(display-info ff)
(define fg (div-interval f g)) ;A/B
(display-info fg)
(newline)
(center (part1 ff fg))
(center (part2 ff fg))
;上述结果不相等,lem正确

;exercise-2.15
#|
在上面的习题2.14中，par1中R1与R2各出现了两次,因为R1与R2的值都是不确定的,多次引用的话肯定会使最终的结果误差较大.比如：R1的区间范围为[2,4],那么R1第一次取值可能为2.5,第二次取值可能为3.5.这在实际中是不可能的,虽然R1的值是不确定的,但是只要R1的值定下来了,后面引用多少次都是一样的而在part2中R1和R2只引用了一次这样结果误差会更小
|#

;exercise-2.16
;n(m+l)= nm + nl
(define n (make-interval 2 4))
(define m (make-interval -2 0))
(define l (make-interval 3 8))

(define o (mul-interval n
                        (add-interval m l)))
(define p (add-interval (mul-interval n m)
                        (mul-interval n l)))
(display-info o)
(display-info p)
(display-info (div-interval (mul-interval n n) n)) ; n*n/n = n 算术代数上相等但是计算结果不相等;
;问题2:因为区间是每次随机取值,每次不确定的因数越多结果误差越大,没有办法改变不确定因素的情况下是无法做到这样的设计程序的


















