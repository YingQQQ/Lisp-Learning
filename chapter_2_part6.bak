#lang sicp
;符号数据

(define a 1)
(define b 2)
(define c 3)
(list a b)
(list 'a b)
(list 'a 'b c)
(car '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

;exercise-2.53
(list 'a 'b 'c); (a b c)
(list (list 'george)); ((george))
(cdr '((x1 x2) (y1 y2))); ((y1 y2))
(cadr '((x1 x2) (y1 y2))); (y1 y2)
(pair? (car '(a short list))); #f
(memq 'red '((red shoes) (blue socks))); #f
(memq 'red '(red shoes blue socks)); (red shoes blue socks)

;exercise-2.54

(define (equal? item x)
  (if (and (pair? item) (pair? x)) ;item, x 都是list
      (if (and (equal? (car item) (car x)))
           (equal? (cdr item) (cdr x))
           false)
      (eq? item x)))

(equal? '(list is a list) '(list is a list))

(equal? '(list is a list) '(list (is a) list))

;exercise-2.55
(car ''abracdadad)
;根据97的100注释可以得知'就quote的缩写形式

;2.3.2 实例:符号求导

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression tyep -- DERIV" exp))))

(define (variable? exp)
  (symbol? exp));symbol 判断是不是符号

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend a)
  (cadr a))

(define (augend a)
  (caddr a))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier a)
  (cadr a))

(define (multiplicand a)
  (caddr a))

;对一些特殊运算的简化
(define (make-sum-better a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product-better m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;exercise-2.56

(define (deriv-exercise exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv-exercise (addend exp) var)
                   (deriv-exercise (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-exercise (multiplicand exp) var))
          (make-product (deriv-exercise (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv-exercise (base exp) var)))
        (else
         (error "unknown expression tyep -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent p)
  (caddr p))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

;exercise-2.57

(define (augend-exercise a)
  (if (null? (cdddr a))
      (caddr a)
      (cons '+ (cddr a))))

(define (multiplicand-exercise p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;exercise-2.58

;a)
(define (make-sum-new a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum-new? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-new a)
  (car a))

(define (make-product-new m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))


(define (product-new? x)
  (and (pair? x) (eq? (cadr x) '*)))


(define (multiplier-new a)
  (car a))

;b)
;如果允许使用标准代数写法的话，那么我们就没办法只是通过修改谓词、选择函数和构造函数来达到正确计算如果允许使用标准代数写法的话,那么我们就没办法只是通过修改谓词、选择函数和构造函数来达到正确计算求导的目的,因为这必须要修改 deriv 函数,提供符号的优先级处理功能。
;比如说,对于输入 x + y * z ,有两种可能的求导顺序会产生(称之为二义性文法),一种是 (x + y) * z ,另一种是 x + (y * z) ;对于求导计算来说,后一种顺序才是正确的,但是这种顺序必须通过修改 deriv 来提供,只是修改谓词、选择函数和构造函数是没办法达到调整求导顺序的目的的。






























