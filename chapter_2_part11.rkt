#lang sicp
;;2.4抽象数据的多重表示
;无论复数如何定义,都可以定义通用性的运算规则

;;2.41 复数的表示

(define (add-complex z1 z2);坐标系实数部分和虚数部分相加
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2);坐标系实数部分和虚数部分相减
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2);极标系实数部分摸相乘,虚数部分角度相加
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

(define (div-complex z1 z2);极标系实数部分摸相除,虚数部分角度相减
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

;;2.42 带标志的数据
(define (attach-tag type-tag contents)
  (cons type-tag contents));以一个标志和实际内容为参数生产一个带标志的数据对象

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)));获取标志

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONENTS" datum)));获取内容

(define (rectangular? z)
  (eq? (type-tag z) 'reactangular));判断是否是直角坐标系

(define (polar? z)
  (eq? (type-tag z) 'polar));判断是否是极坐标

;x = r cos A, y = r sin A

(define (real-part-polar z);通过极坐标获取直角坐标x点的位置
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z) ;通过极坐标获取直角坐标y点的位置
  (* (magnitude-polar z) (sin (angle-polar))))

(define (magnitude-polar z)
  (car z));获取极坐标的摸长度
(define (angle-polar z)
  (cdr z));获取极坐标的角度

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))));用直角坐标系构造对象

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)));用极坐标构造对象


(define (real-part z);获取直角坐标系实数部分
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

  
(define (imag-part z);获取直角坐标系虚数部分
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))


(define (magnitude z);获取极坐标中的摸
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))


(define (angle z);获取极坐标中的角度
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))


(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (square x)
  (* x x))
































































      