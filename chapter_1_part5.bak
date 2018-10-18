#lang sicp
; chapter-1.3.3
;区间折半寻找方程的根,所需步数的增长是O(log(L/T)),
;其中L是区间的初始长度,T是可以容忍的误差(即认为'足够小的'的区间大小)

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.01))

(define (average a b)
  (/ (+ a b) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
(half-interval-method sin 2.0 4.0)

(exact->inexact (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1
                      2))
;函数的不动点, f(x) = x
; f(f(f(x)))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point
 (lambda (y)(+ (sin y) (cos y)))
 1.0)

;exercise 1.35 黄金分割点

(fixed-point
 (lambda (x) (+ 1 (/ 1 x)))
 1.0)

; exercise 1.36

(define (new-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (infos text)
    (display "infos:")
    (display text)
    (newline)
    )
  (define (try guess)
    (infos guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;steps ≈ 35
(new-fixed-point
 (lambda (x) (/ (log 1000) (log x)))
 2.0)

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2
       )))
;steps ≈ 10
(new-fixed-point
 (average-damp (lambda (x) (/ (log 1000) (log x))))
 2.0)

;exercise 1.37
;a)

;递归版本
(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i)
           (+ (d i) (cf (+ i 1))))
        )
    )
  (cf 1))
;; 十进制前四位0.6180
(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           11); ≈ 0.6180555555555556 前四位已经相同

;迭代版本
(define (fast-cont-frac n d k)
  (define (cf i result)
    (if (= i 0)
        result
        (cf (- i 1)
            (/ (n i) (+ (d i) result) ))
        )
    )
  (cf (- k 1) (/ (n k) (d k)))
  )
;; 十进制前四位0.6180
(fast-cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           11); ≈ 0.6180555555555556 结果相同


;exercise 1.38

(define (de k)
  (define (n i)
    1)
  (define (even? i)
    (= (remainder (+ i 1) 3) 0))
  (define (d k)
    (if (even? k)
        (* 2 (/ (+ k 1) 3))
        1))
  (+ 2 (fast-cont-frac n d k)))

(exact->inexact (de 100))

;exercise 1.39
;Lambert
;递归版本
(define (tan-cf x k)
  (define (d i)
    (- (* 2 i) 1))
  (define (n i)
    (if (= i 1)
        x
        (* x x)))
  (define (cf i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i)
           (- (d i)
              (cf(+ i 1))))
        )
    )
  (cf 1))
(exact->inexact (tan-cf 10
  1000))
(tan 10);与内置的tan函数比较,校验正确值

; chapter-1-1.34 page-48

(define (sqrt x)
  (define (average-damp f)
    (lambda (x) (average x (f x))))
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;newton-transform
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


(define (cube x)
  (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (new-sqrt x)
  (newton-method (lambda (y) (- (* y y) x))
                 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-first x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (sqrt-second x)
  (fixed-point-of-transform (lambda (y) (- (* y y) x))
                            newton-transform
                            1.0))

;exercise-1.40

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(newton-method (cubic 3 2 1) 1)

;exercise-1.41

(define (double f)
    (lambda (x)
        (f (f x))))

(((double (double double)) inc) 5) ; = 21

; exercise-1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (square x)
  (* x x))

((compose square inc) 6)

;exercise-1.43

(define (repeated n f)
  (if (= n 1)
      f
      (lambda (x)
        (let ((fs (repeated (- n 1) f)))
          (f (fs x))))))

(define (fast-repeated n f)
  (define (iter i rf)
    (if (= i 1)
        rf
        (iter (- i 1)
              (lambda (x)
                (f (rf x))))))
  (iter n f))

(define (repeated-compose n f)
  (if (= n 1)
      f
      (compose f (repeated-compose (- n 1) f))))

(define (fast-repeated-compose n f)
  (define (iter i rf)
    (if (= i 1)
        rf
        (iter (- i 1)
              (compose f rf))))
  (iter n f))

((repeated 2 square) 5)
((fast-repeated 2 square) 5)

;exercise-1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
((smooth square) 5)

(((fast-repeated 10 smooth) square) 5)

(define (smooth-times n f)
  (let ((smooth-time-f (fast-repeated n smooth)))
    (smooth-time-f f)))

((smooth-times 10 square) 5)


;exercise-1.45

(define (expt base n)
  (if (= n 0)
      1
      ((fast-repeated n (lambda (x) (* base x))) 1)))

(expt 2 0)
(expt 2 5)

(define (average-damp-n-times n f)
  ((fast-repeated n average-damp) f))


(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point (average-damp-n-times damp-times
                                       (lambda (y)
                                         (/ x
                                            (expt y (- n 1)))))
                 1.0)))

(define sqrt-times (damped-nth-root 2 2))

((damped-nth-root 1 1) 3)

((damped-nth-root 2 2) (* 3 3))

((damped-nth-root 3 3) (* 3 3 3))

((damped-nth-root 4 2) (* 3 3 3 3))
((damped-nth-root 5 2) (* 3 3 3 3 3))
((damped-nth-root 5 3) (* 3 3 3 3 3))
