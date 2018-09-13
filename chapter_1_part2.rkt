#lang sicp
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))
;;费马小定律
(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod  base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod  base (- exp 1) m))
                         m))))

;; random 函数包含0本身,不包含n本身故此+1防止从0开始
(define (farmat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;;需要增加次数的原因是通过费马定律的数不一定是素数,但是不通过一定不是素数.而n通过这一检验只能作为
;;它是素数的一个强有力证据,但是却不是n为素数的保证,我们能说的是,对于任何n,如果执行这一检查的次数足够多
;;而且都通过检查,那么就能使这一素数的检查出错概率小到所需要的任意程度......
;;后续解释page-35
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((farmat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

;;exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
;;execrise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))
(define (add? n)
  (= (remainder n 2) 1))

(define (next-odd n)
  (if (add? n)
      (+ 2 n)
      (+ 1 n)))



