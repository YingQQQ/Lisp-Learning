#lang sicp
;;实例:Huffman编码树

(define (make-leaf symbol weight)
  (list 'leaf symbol weight));构造函数

(define (leaf? object)
  (eq? (car object) 'leaf));判断是否是leaf

(define (symbol-leaf x)
  (cadr x));获取符号集合过程

(define (weight-leaf x)
  (caddr x));获取权重过程

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))));构建Huffman编码树

(define (left-branch tree)
  (car tree));获取左分支

(define (right-branch tree)
  (cadr tree));获取右分支

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)));获取符号

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)));获取权重

;解码过程
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits);空集合
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0)
         (left-branch branch))
        ((= bit 1)
         (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


;带权重元素的集合

(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;exercise-2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(decode sample-message sample-tree);(A D A B B C A)

;exercise-2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
               (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((contains? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? symbol (right-branch tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else
         (error "unkonwn bit symbol:" symbol))))

(define (contains? symbol tree)
  (define (search symbol lst)
    (cond ((null? lst)
           false)
          ((eq? symbol (car lst))
           true)
          (else (search symbol (cdr lst)))))
  (search symbol (symbols tree)));symbols 取出所有符号

(contains? 'D sample-tree) ; true
(contains? 'E sample-tree) ; false


(encode '(A D A B B C A) sample-tree) ;(0 1 1 0 0 1 0 1 0 1 1 1 0)

;exercise-2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge pairs)
  (cond ((null? pairs)
         '())
        ((= (length pairs) 1)
         (car pairs))
        (else
         (successive-merge
          (adjoin-set (make-code-tree (car pairs)
                                      (cadr pairs));构建Huffman编码树
                      (cddr pairs))))))


;exercise-2.70

(define music-words '((na 16) (yip 9) (Sha 3) (a 2) (Get 2) (job 2) (boom 1) (Wah 1)))

(define music-tree (generate-huffman-tree music-words))

(define music-message
  '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(define music-bits (encode music-message music-tree))

(length music-bits);84

;如果要选择定长编码则;一共36次 每个3个二进制位; 36*3 = 108;
;108 - 84 = 24;使用 huffman 编码比使用定长编码节省了 24 个二进制位。


;exercise-2.71
#|
n = 5 ==> 1 2 4 8 16
        *
       /\
      *  16
     /\
    *  8
   / \
  *   4
 /\
1  2
n=10 时的树
                  *
                 /\
                *  512
               /\
              *  256
             /\
            * 128
           /\
          *  64
         /\
        *  32
       /\
      *  16
     /\
    *  8
   / \
  *   4
 /\
1  2
可以看出，对于这种类型的树，编码使用最频繁的字符需要 1个二进制位，而编码最不常用的字符需要 n−1 个二进制位。
|#

;exercise-2.72

#|
如果符号的相对频度跟 练习 2.71 所列举的一样，那么根据 练习 2.71 的结果，对于出现最频繁的字符，每次编码它需要下降 1 层，而对于出现最不频繁的字符，每次编码它需要下降 n−1 层。

因此，如果编码字符的次数为 n ，那么对最频繁出现的字符进行编码的复杂度为 Θ(n) ，而对最不频繁出现的字符进行编码的复杂度为 Θ(n2) 。
|#





















































      