#lang racket

(provide (prefix-out anim- (all-from-out 'animations))
         (prefix-out bg- (all-from-out 'backgrounds)))

(module animations racket
  (require 2htdp/image)
  (provide table)
  (define table
    (list (cons "Character Facing Right" (list (bitmap "pngs/Main Character-Standing-0.png")))
          (cons "Character Facing Left" (list (flip-horizontal (bitmap "pngs/Main Character-Standing-0.png"))))
          (cons "Character Attack Right" (list (bitmap "pngs/Main Character-Attack-alt.png")))
          (cons "Character Attack Left" (list (flip-horizontal (bitmap "pngs/Main Character-Attack-alt.png"))))
          (cons "Enemy Facing Right" (list (bitmap "pngs/Bat-Flying-0.png")(bitmap "pngs/Bat-Flying-1.png")))
          (cons "Enemy Facing Left" (list (flip-horizontal (bitmap "pngs/Bat-Flying-0.png"))
                                            (flip-horizontal (bitmap "pngs/Bat-Flying-1.png"))))))
  (void))

(module backgrounds racket
  (require 2htdp/image)
  (provide room-background)
  (define (tile image x y)
    ;; Generate the rows
    (let ([row (foldl (lambda (i out) (overlay/xy out i 0 image))
                      empty-image
                      (map (lambda (i) (* i (image-width image))) (range x)))])
      ;; Generate the entire image based on the rows
      (foldl (lambda (i out) (overlay/xy out 0 i row))
             empty-image
             (map (lambda (i) (* i (image-height image)))
                  (range y)))))
  
  
  (define single-unit-room-tile
    (underlay/xy (tile (bitmap "pngs/Level Tiles-Wall-0.png") 20 10)
                 270 64
                 (bitmap "pngs/Window.png")))
  
  (define single-unit-floor
    (tile (bitmap "pngs/Level Tiles-Floor-0.png") 20 1))
  
  (define (room-background x)
    (underlay/xy (tile single-unit-room-tile x 1)
                 0 288
                 (tile single-unit-floor x 1))))

(require 'backgrounds)
(require 'animations)
