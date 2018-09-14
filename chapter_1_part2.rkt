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
;;由于现代计算机性能强劲我们用比较大的数来测试,第一次我们用count=3来测试
;;(search-for-primes 1e8 3) ;; 5000
;;(search-for-primes 1e9 3) ;;13000
;;(search-for-primes 1e10 3) ;; time不是很稳定有时候大有时候小(73000 -- 161000)
;;第二次我们用count=1来测试
;;(search-for-primes 1e8 1) ;; 1000
;;(search-for-primes 1e9 1) ;; 5000
;;(search-for-primes 1e10 1) ;; 28000
;;结论就是随着基数增加10倍,消耗的时间并不是绝对呈现√10的关系

;;execrise 1.23
(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))

(define (fast-find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (fast-find-divisor n (next-divisor? test-divisor)))))

(define (next-divisor? test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (faster-prime? n)
  (= n (fast-smallest-divisor n)))

(define (fast-continues-primes n count)
  (cond ((= count 0)
         (display "primes"))
        ((faster-prime? n)
         (display n)
         (newline)
         (fast-continues-primes (next-odd n) (- count 1)))
        (else (fast-continues-primes (next-odd n) count))))

(define (fast-search-for-primes n count)
  (let (( start-time (runtime)))
    (fast-continues-primes n count)
    (newline)
    (display "fast-times=")
    (- (runtime) start-time)))
#|
由于现代计算机性能强劲我们用比较大的数来测试,第一次我们用count=3来测试
(search-for-primes 1e8 3) ;; 5000
(fast-search-for-primes 1e8 3) ;; 7000
(search-for-primes 1e9 3) ;; 49000
(fast-search-for-primes 1e9 3) ;; 16000
(search-for-primes 1e10 3) ;; 91000
(fast-search-for-primes 1e10 3) ;; 57000
第二次我们用count=1来测试
(search-for-primes 1e8 1) ;; 2000
(fast-search-for-primes 1e8 1) ;; 1000
(search-for-primes 1e9 1) ;; 6000
(fast-search-for-primes 1e9 1) ;; 3000
(search-for-primes 1e10 1) ;; 47000
(fast-search-for-primes 1e10 1) ;; 13000
测试结果虽然速度有所上升,但提升的速度并不是严格地按照书中所说的那样，按一倍的速度增长
|#

;; execrise 1.24





