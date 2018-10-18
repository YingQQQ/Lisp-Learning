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









