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

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;exercise-2.46
(define (make-vect x y)
  (cons x y))

(define (make-vect-2 x y)
  (list x y));list

(define (xcor-vect r)
  (car r))

(define (ycor-vect r)
  (cdr r))

(define (ycor-vect-2 r)
  (cadr r));list

(define (add-vect r1 r2)
  (make-vect (+ (xcor-vect r1) (xcor-vect r2))
             (+ (ycor-vect r1) (ycor-vect r2))))

(define (sub-vect r1 r2)
  (make-vect (- (xcor-vect r1) (xcor-vect r2))
             (- (ycor-vect r1) (ycor-vect r2))))

(define (scale-vect r s)
  (make-vect (* (xcor-vect r) s)
             (* (ycor-vect r) s)))

;execrise-2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-2 frame)
  (car frame))

(define (edge1-2 frame)
  (cadr frame))

(define (edge2-2 frame)
  (cddr frame))


;;画家

(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
     

;exercise-2.48

(define (make-segment x y)
  (list x y))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cadr l))


;exercise-2.49

(define top-left (make-vect 0.0 1.0))
(define top-right (make-vect 1.0 1.0))
(define bottom-left (make-vect 0.0 0.0))
(define bottom-right (make-vect 1.0 0.0))

(define top (make-segment top-left top-right))
(define left (make-segment top-left bottom-left))
(define right (make-segment top-right bottom-right))
(define bottom (make-segment bottom-left bottom-right))

(segment->painter (list top bottom left right))

;b)

(define left-top-to-right-bottom
  (make-segment top-left bottom-right))

(define left-top-to-left-bottom
  (make-segment top-right bottom-left))

(segment->painter (list left-top-to-right-bottom left-top-to-left-bottom))

;c)菱形需要各边的中点相连
(define (center l)
  (make-vect (/ (car l) 2)
             (/ (cdr l) 2)))

(define mid-top (make-segment (center top-left) (center top-right)))
(define mid-left (make-segment (center top-left) (center bottom-left)))
(define mid-right (make-segment (center top-right) (center bottom-right)))
(define mid-bottom (make-segment (center bottom-left) (center bottom-right)))

(segment->painter (list mid-top mid-left mid-right mid-bottom))

;d)
(segments->painter (list
                         (make-segment (make-vect 0.4 1.0)      ; 头部左上
                                       (make-vect 0.35 0.85))
                         (make-segment (make-vect 0.35 0.85)    ; 头部左下
                                       (make-vect 0.4 0.64))
                         (make-segment (make-vect 0.4 0.65)     ; 左肩
                                       (make-vect 0.25 0.65))
                         (make-segment (make-vect 0.25 0.65)    ; 左手臂上部
                                       (make-vect 0.15 0.6))
                         (make-segment (make-vect 0.15 0.6)     ; 左手上部
                                       (make-vect 0.0 0.85))

                         (make-segment (make-vect 0.0 0.65)     ; 左手下部
                                       (make-vect 0.15 0.35))
                         (make-segment (make-vect 0.15 0.35)    ; 左手臂下部
                                       (make-vect 0.25 0.6))

                         (make-segment (make-vect 0.25 0.6)     ; 左边身体
                                       (make-vect 0.35 0.5))
                         (make-segment (make-vect 0.35 0.5)     ; 左腿外侧
                                       (make-vect 0.25 0.0))
                         (make-segment (make-vect 0.6 1.0)      ; 头部右上
                                       (make-vect 0.65 0.85))
                         (make-segment (make-vect 0.65 0.85)    ; 头部右下
                                       (make-vect 0.6 0.65))
                         (make-segment (make-vect 0.6 0.65)     ; 右肩
                                       (make-vect 0.75 0.65))
                         (make-segment (make-vect 0.75 0.65)    ; 右手上部
                                       (make-vect 1.0 0.3))

                         (make-segment (make-vect 1.0 0.15)     ; 右手下部
                                       (make-vect 0.6 0.5))
                         (make-segment (make-vect 0.6 0.5)      ; 右腿外侧
                                       (make-vect 0.75 0.0))

                         (make-segment (make-vect 0.4 0.0)      ; 左腿内侧
                                       (make-vect 0.5 0.3))
                         (make-segment (make-vect 0.6 0.0)      ; 右腿内侧
                                       (make-vect 0.5 0.3))))
;画家的变换和组合
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter) ;上下翻转
  (transform-painter painter
                     (make-vect 0.0 1.0);new origin
                     (make-vect 1.0 1.0);new end of edge1
                     (make-vect 0.0 0.0)));new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5);new origin
                     (make-vect 1.0 0.5);new end of edge1
                     (make-vect 0.5 1.0)));new end of edge2

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0);new origin
                     (make-vect 1.0 1.0);new end of edge1
                     (make-vect 0.0 0.0)));new end of edge2

(define (square-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0);new origin
                     (make-vect 0.65 0.35);new end of edge1
                     (make-vect 0.35 0.65)));new end of edge2

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((painter-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (painter-right
           (transform-painter painter2
                              split-point
                              (make-vect 0.5 1.0)
                              (make-vect 1.0 1.0))))
      (lambda (frame)
        (painter-left frame)
        (painter-right frame)))))

;exercise-2.50

(define (flip-horiz painter) ;flip-hozil 使用一个 painter，画出左右反转后的图
  (transform-painter painter
                     (make-vect 1.0 0.0);new origin
                     (make-vect 0.0 0.0);new end of edge1
                     (make-vect 1.0 1.0)));new end of edge2

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0);new origin
                     (make-vect 0.0 1.0);new end of edge1
                     (make-vect 1.0 0.0)));new end of edge2

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0);new origin
                     (make-vect 0.0 0.0);new end of edge1
                     (make-vect 1.0 1.0)));new end of edge2

;exercise-2.51

(define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-top
                (transform-painter painter2
                                   split-point
                                   (make-vect 1.0 0.5)
                                   (make-vect 0.0 1.0)))
              (paint-down
                (transform-painter painter1
                                   (make-vect 0.0 0.0)
                                   (make-vect 1.0 0.0)
                                   split-point)))
            (lambda (frame)
                (paint-top frame)
                (paint-down frame)))))


;exercise-2.52

;a)
(segments->painter (list (make-segment (make-vect 0.0 0.0)
                                       (make-vect 1.0 1.0))
                         (make-segment (make-vect 0.4 1.0)      ; 头部左上
                                       (make-vect 0.35 0.85))
                         (make-segment (make-vect 0.35 0.85)    ; 头部左下
                                       (make-vect 0.4 0.64))
                         (make-segment (make-vect 0.4 0.65)     ; 左肩
                                       (make-vect 0.25 0.65))
                         (make-segment (make-vect 0.25 0.65)    ; 左手臂上部
                                       (make-vect 0.15 0.6))
                         (make-segment (make-vect 0.15 0.6)     ; 左手上部
                                       (make-vect 0.0 0.85))

                         (make-segment (make-vect 0.0 0.65)     ; 左手下部
                                       (make-vect 0.15 0.35))
                         (make-segment (make-vect 0.15 0.35)    ; 左手臂下部
                                       (make-vect 0.25 0.6))

                         (make-segment (make-vect 0.25 0.6)     ; 左边身体
                                       (make-vect 0.35 0.5))
                         (make-segment (make-vect 0.35 0.5)     ; 左腿外侧
                                       (make-vect 0.25 0.0))
                         (make-segment (make-vect 0.6 1.0)      ; 头部右上
                                       (make-vect 0.65 0.85))
                         (make-segment (make-vect 0.65 0.85)    ; 头部右下
                                       (make-vect 0.6 0.65))
                         (make-segment (make-vect 0.6 0.65)     ; 右肩
                                       (make-vect 0.75 0.65))
                         (make-segment (make-vect 0.75 0.65)    ; 右手上部
                                       (make-vect 1.0 0.3))

                         (make-segment (make-vect 1.0 0.15)     ; 右手下部
                                       (make-vect 0.6 0.5))
                         (make-segment (make-vect 0.6 0.5)      ; 右腿外侧
                                       (make-vect 0.75 0.0))

                         (make-segment (make-vect 0.4 0.0)      ; 左腿内侧
                                       (make-vect 0.5 0.3))
                         (make-segment (make-vect 0.6 0.0)      ; 右腿内侧
                                       (make-vect 0.5 0.3))))
;b)
(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1)))
              (corner (corner-split painter (- n 1))))
            (beside (below painter up)
                    (below right corner)))))

;c)
(define (square-limit painter n)
    (let ((combine4 (square-of-four identity flip-horiz)
                                    flip-vect rotate180))
        (combine4 (corner-split painter n))))


























                     
