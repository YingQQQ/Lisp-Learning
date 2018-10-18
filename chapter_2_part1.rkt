#lang sicp
(define (add-rat x y)
  (make-rat (+ (* (number x) (denom y))
               (* (number y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (number x) (denom y))
               (* (number y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (number x) (number y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (number x) (denom y))
            (* (number y) (denom x))))

(define (equal-rat? x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))

(define x (cons 1 2))
(car x)
(cdr x)
(define y (cons 3 4))
(define z (cons x y))
(cdr (car z)) ;2


(define (make-rat n d)
  (cons n d))
(define (number x)
  (car x))
(define (denom y)
  (cdr y))

(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))


;exercise-2.1
(define (exercise-make-rat n d)
  (if (< d 0)
      (cons (- n) (- d));负负得正
      (cons n d)))

;exercise-2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (make-segment  start end)
  (cons start end))

(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point y)
  (cdr y))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define p1 (make-point 1 2))
(print-point p1)
(define p2 (make-point 3 4))
(print-point p2)

(define (average x y)
  (/ (+ x y) 2))
(define (midpoint-segment x)
  (let ((start (start-segment x))
        (end (end-segment x)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

(define seg (make-segment p1 p2))

(print-point (midpoint-segment seg))


;exercise-2.3
;; 用点的方式来构建, 一个矩形有四个点
(define (make-rectangle l1 l2 w1 w2)
  (cons (cons l1 l2)
        (cons w1 w2)))

(define (length-1-rectangle r)
  (car (car r))) ;l1

(define (length-2-rectangle r)
  (cdr (car r))) ;l2

(define (width-1-rectangle r)
  (car (cdr r))) ;w1

(define (width-2-rectangle r)
  (cdr (cdr r))) ;w2

(define l1 (make-segment (make-point 1 1)
                         (make-point 3 1)))
(define l2 (make-segment (make-point 1 3)
                         (make-point 3 3)))
(define w1 (make-segment (make-point 1 1)
                         (make-point 1 3)))
(define w2 (make-segment (make-point 3 1)
                         (make-point 3 3)))

(define rectangle (make-rectangle l1 l2 w1 w2))

(define (length-of-rectangle r)
  (let ((length (length-1-rectangle r)))
    (let ((start-seg (start-segment length))
          (end-seg (end-segment length)))
      (abs (- (x-point start-seg)
              (x-point end-seg))))))
(newline)
(length-of-rectangle rectangle)

(define (width-of-rectangle r)
  (let ((width (width-1-rectangle r)))
    (let ((start-seg (start-segment width))
          (end-seg (end-segment width)))
      (abs (- (y-point start-seg)
              (y-point end-seg))))))
(newline)
(width-of-rectangle rectangle)

(define (perimeter-rectangle r)
  (let ((length (length-of-rectangle r))
        (width (width-of-rectangle r)))
    (* 2 (+ length width))))
(newline)   
(perimeter-rectangle rectangle)

(define (area-rectangle r)
    (let ((length (length-of-rectangle r))
        (width (width-of-rectangle r)))
    (* length width)))

(newline)   
(area-rectangle rectangle)

;;用线段来构建,一个矩形有2条线段
(define (make-another-rectangle l w)
  (cons l w))
(define (length r)
  (car r))
(define (width r)
  (cdr r))

(define l (make-segment (make-point 1 1)
                        (make-point 3 1)))

(define w (make-segment (make-point 1 1)
                        (make-point 1 3)))

(define another-rectangle (make-another-rectangle l w))

(define (another-length-of-rectangle r)
    (let ((length (length r)))
        (let ((start (start-segment length))
              (end (end-segment length)))
            (- (x-point end)
               (x-point start)))))
(newline)   
(another-length-of-rectangle another-rectangle)




