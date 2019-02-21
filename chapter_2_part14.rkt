#lang sicp

;;消息传递
;exericse-2.75

(define (make-from-real-imag r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude)
                r)
          ((eq? op 'angle)
                a)
              (else
                (error "Unkonw op  -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;exercise-2.76
;https://sicp.readthedocs.io/en/latest/chp2/76.html

;2.5 带有通用型操作的系统

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


(define  (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
 (cond ((number? datum)
        'scheme-number)
       ((pair? datum)
        (car datum))
       (else
            (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
    (cond ((number? datum)
            datum)
          ((pair? datum)
            (cdr datum))
          (else
            (error "Bad tagged datum -- CONTENT" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (write type-tags)
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No mathod for these types --- APPLY-GENERIC"
           (list op type-tags))))))
;通用型算术运算
(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x y) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;有理数程序包
(define (install-rational-package)
  ;;internal-procedures
  (define (number x)
    (car x))
  (define (denom x)
    (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
              (* (denom y) (denom x))))
  (define (div-rat x y)
    (make-rat (* (number x) (denom y))
              (* (denom x) (number y))))

  ;;inrerface to rest of the system

  (define (tag x)
    (attach-tag 'rantional x))
  (put 'add '(rantional rantional)
       (lambda (x y)
         (tag (add-rat x y))))
  (put 'sub '(rantional rantional)
       (lambda (x y)
         (tag (sub-rat x y))))
  (put 'mul '(rantional rantional)
       (lambda (x y)
         (tag (mul-rat x y))))
  (put 'div '(rantional rantional)
       (lambda (x y)
         (tag (div-rat x y))))
  (put 'make 'rantional
       (lambda (n d)
         (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;复数程序包

(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                        (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                        (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;;interface to rest of system

  (define (tag z)
    (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;;exercise-2.77
;;https://sicp.readthedocs.io/en/latest/chp2/77.html

;;exercise-2.78
;;https://sicp.readthedocs.io/en/latest/chp2/78.html

;;exercise-2.80
;;https://sicp.readthedocs.io/en/latest/chp2/80.html
















  
  










