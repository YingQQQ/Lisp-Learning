#lang sicp
;;exercise-2.73

#|
old func
(define (deriv exp var)
  (cond ((number? exp)
         0)
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
                        (multiplier exp))))
        ;;more rules
        (else
         (error "unknown expression type -- DERIV" exp))))
|#

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get `deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
    'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
  
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
#|
a)
deriv 过程没有对 number? 和 same-variable? 使用数据导向处理的原因是,在求导程序中,数字被直接表示为 Scheme 的数值类型,而变量被直接表示为 Scheme 的符号类型(查看书本 100 页),因此只使用 number? 和 same-variable? 这两种内置的谓词语句,就足以对这两中类型进行判断了,没有必要画蛇添足。
|#

;b)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum --- CONTENTS" datum)))

(define (install-sum-package)

    ;;; internal procedures 
    (define (addend s)
        (car s))

    (define (augend s)
        (cadr s))

    (define (make-sum x y)
        (cond ((=number? x 0)
                y)
              ((=number? y 0)
                x)
              ((and (number? x) (number? y))
                (+ x y))
              (else
                (attach-tag '+ x y))))

    ;;; interface to the rest of the system
    (put 'addend '+ addend)
    (put 'augend '+ augend)
    (put 'make-sum '+ make-sum)

    (put 'deriv '+
        (lambda (exp var)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var))))

'done)

(define (make-sum x y)
    ((get 'make-sum '+) x y))

(define (addend sum)
    ((get 'addend '+) (contents sum)))

(define (augend sum)
    ((get 'augend '+) (contents sum)))

(install-sum-package)

;c)
(define (make-product m1 m2)
  (list '* m1 m2))

(define (install-exponentiation-package)
  (define (base exp)
    (car exp))
  (define (exponent exp)
    (cadr exp))
  (define (make-exponentiation base n)
    (cond ((= n 0)
           1)
          ((= n 1)
           base)
          (else
           (attach-tag '** base n))))
  (put 'base '** base)
  (put 'exponent '** exponent)
  (put 'make-exponentiation '** make-exponentiation)
  (put 'deriv '**
       (lambda (exp var)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product
            n
            (make-product
             (make-exponentiation
              u
              (- n 1))
             (deriv u var))))))
  'make-exponentiation-done)
(install-exponentiation-package)

;d)
;只需要相应的改变put和get中选择的顺序即可
;(put 'make-sum '+ make-sum) => (put '+ 'make-sum make-sum)



;exercise-2.74
;初始化雇员的资料文件
(define (make-generic-employee-file division employee-file)
  (cons division employee-file))
;获取机构
(define (division generic-employee-file)
  (car generic-employee-file))
;获取文件
(define (employee-file generic-employee-file)
  (cdr generic-employee-file))

(define (get-record employee-name generic-employee-file)
  ((get 'get-record (division generic-employee-file))
   employee-name
   (employee-file generic-employee-file)))

;; b.

(define (get-salary generic-employee-record)
  ((get 'get-salary (division generic-employee-record))
     (employee-record generic-employee-record)))

;; c.

(define (find-employee-record employee-name generic-files)
  (if (null? generic-files)
      (error "Record not found")
      (let ((record (get-record employee-name (car generic-files))))
        (if (record)
            record
            (find-employee-record employee-name (cdr generic-files))))))






















        