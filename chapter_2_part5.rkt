#lang sicp
;;2.24  实例: 一个图形语言

;exercise-2.44
;below 使用两个 painter，让它们分别在上下两个半区画图
;beside 使用两个 painter，让它们分别在左右两个半区画图
;flip-vert 使用一个 painter，画出上下反转后的图
;flip-hozil 使用一个 painter，画出左右反转后的图
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter
               (baside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; 高阶操作
;t1左上角,tr右上角,b1左下角,br右下角
(define (square-of-four t1 tr b1 br)
  (lambda (painter)
    (let ((top (beside (t1 painter) (tr painter)))
          (bottom (beseide (b1 painter) (br painter))))
      (below top bottom))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;exercise-2.45

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter
               (baside smaller smaller)))))


(define right-split (split beside below))

(define (split big-combiner small-combiner)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split big-combiner small-combiner) painter (- n 1))))
          (big-combiner painter
                      (small-combiner smaller smaller))))))

(define (split-short big-combiner small-combiner)
    (define (inner painter n)
        (if (= n 0)
            painter
            (let ((smaller (inner painter (- n 1))))
                (big-combiner painter   
                              (small-combiner smaller smaller)))))
    inner)


;;框架













  