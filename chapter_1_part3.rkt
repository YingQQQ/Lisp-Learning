#lang sicp
;; execrise 1.24
(define (square x)
  (* x x))
(define (even? n)
  (= (remainder n 2) 0))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0)
            true)
          ((fermat-test n)
            (fast-prime? n (- times 1)))
          (else
            false)))

(define (expmod base exp m)
    (cond ((= exp 0)
            1)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                       m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))
(define (prime? n)
    (fast-prime? n 10))

(define (add? n)
  (= (remainder n 2) 1))

(define (next-odd n)
  (if (add? n)
      (+ 2 n)
      (+ 1 n)))

(define (continues-primes n count)
  (cond ((= count 0)
         (display "primes"))
        ((prime? n)
         (display n)
         (newline)
         (continues-primes (next-odd n) (- count 1)))
        (else (continues-primes (next-odd n) count))))

(define (search-for-primes n count)
  (let (( start-time (runtime)))
    (continues-primes n count)
    (newline)
    (display "times=")
    (- (runtime) start-time)))
(search-for-primes 100000000 1)
(search-for-primes 1000000000 1)
#|
由于现代计算机速度太快,很难测试到当基数增加10倍的时候,计算所需的时间并不是按预期的那样，严格地按常数增长
|#

;; execrise 1.25

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))
#|
(expmod 1000000000 100000000 3)
(new-expmod 1000000000 100000000 3)
因为费马检查在对一个非常大的数进行素数检测的时候，可能需要计算一个很大的乘幂，比如说
求十亿的一亿次方，这种非常大的数值计算的速度非常慢，而且很容易因为超出实现的限制而造成溢出。
expmod 函数，通过每次对乘幂进行 remainder 操作，从而将乘幂限制在一个很小的范围内（不超过参数 m ）
这样可以最大限度地避免溢出，而且计算速度快得多
|#

;; execrise 1.26
(define (fast-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (fast-expmod base (/ exp 2) m)
                       (fast-expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (fast-expmod base (- exp 1) m))
                         m))))

#|
fast-expmod中当指数是偶数,求余的时候需要计算两次fast-expmod的值因此计算过程是Θ(n)次,
而expmod中当指数是偶数,求余的时候需要计算一次expmod的值因此计算过程是Θ(logn)次,
|#


;; execrise 1.27 Carmichael数 注脚47提供的数字

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)

;; execrise 1.28 Miller-Rabin 检查

(define (miller-expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-square-root? base m)
         0)
        ((even? exp)
         (remainder (square (miller-expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (miller-expmod base (- exp 1) m))
                    m))))

(define (nontrivial-square-root? base m)
  (and (not (= base 1))
       (not (= base (- m 1)))
       (= 1 (remainder (square base) m))))

(define (miller-fast-prime? n times)
  (cond ((= times 0)
         true)
        ((miller-test n)
         (miller-fast-prime? n (- times 1)))
        (else false)))

(define (miller-test n)
  (define (miller-try-it a)
    (= (miller-expmod a (- n 1) n) 1))
  (miller-try-it (+ 1 (random (- n 1)))))
       

(define (miller-prime? n)
    (display "prime is")
    (newline)
    (miller-fast-prime? n 10))

(miller-prime? 4)
(miller-prime? 6)
(miller-prime? 7)
(miller-prime? 561)
(miller-prime? 1105)
(miller-prime? 1729)
(miller-prime? 2465)
(miller-prime? 2821)
(miller-prime? 6601)


