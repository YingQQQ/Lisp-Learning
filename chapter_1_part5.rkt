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

































