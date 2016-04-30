#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "../graphics/Animation.rkt")
(require "../graphics/graphics.rkt")
(require "../audio/audio-generator.rkt")
(require "../level_generation/gen-level-1d.rkt")

(provide run-game)
(provide stop-audio)

;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;

;; Attach a tag to an item
(define (attach-tag tag item)
  (cons tag item))

;; Get an object's tag
(define (get-tag object)
  (if (pair? object)
      (car object)
      (error "get-tag: not a pair")))

;; Untag an object
(define (get-item object)
  (if (pair? object)
      (cdr object)
      (error "get-item: not a pair")))

;; Take a procedure and call it x times
(define (do-x-times x proc )
  (cond ((< x 2) (proc))
        (else
         (do-x-times proc (- x 1))
         (proc))))

;;;;;;;;;;;;;; Coordinate Pair Object ;;;;;;;;;;;;;;

;; Take an x coordinate and a y coordinate
;; and make a coordinate pair
(define (make-coord-pair x y)
  (attach-tag 'CoordPair (cons x y)))

;; Check if an item is a coordinate pair
(define (coord-pair? x)
  (eq? (get-tag x) 'CoordPair))

(define (coord-pair->x coord-pair)
  (car (get-item coord-pair)))

(define (coord-pair->y coord-pair)
  (cdr (get-item coord-pair)))

;;;;;;;;;;;; Game World Object ;;;;;;;;;;;;;;;;

;; Create a game world object
(define (make-game-world
         room
         background
         player-coords
         player-facing
         player-frame-index)
  (attach-tag 'GameWorld
              (list room
                    background
                    player-coords
                    player-facing
                    player-frame-index)))

;; Check if an item is a game world
(define (game-world? x)
  (eq? (get-tag x) 'GameWorld))

;; Accessors
(define (gw->current-room world)
  (first (get-item world)))
(define (gw->background world)
  (second (get-item world)))
(define (gw->player-coords world)
  (third (get-item world)))
(define (gw->player-facing world)
  (fourth (get-item world)))
(define (gw->player-frame-index world)
  (fifth (get-item world)))

;; Consume a world and create a world
;; with the player moved left
(define (gw->move-player-left world distance)
  (if (<= (coord-pair->x (gw->player-coords world)) 0)
      ;; Enter left exit
      (let* ([new-room (send (gw->current-room world) use-exit 'left)]
             [new-background (bg-room-background (send new-room get-width))])
        (make-game-world
         new-room
         new-background
         (make-coord-pair (- (image-width new-background) 50) 227)
         'Left
         (gw->player-frame-index world)))
      ;; Move player left
      (make-game-world
       (gw->current-room world)
       (gw->background world)
       (make-coord-pair
        (- (coord-pair->x (gw->player-coords world)) distance)
        (coord-pair->y (gw->player-coords world)))
       'Left
       (gw->player-frame-index world))))

;; Consume a world and create a world
;; with the player moved right
(define (gw->move-player-right world distance)
  (if (>= (coord-pair->x (gw->player-coords world))
          (- (image-width (gw->background world)) 50))
      ;; Use right exit
      (let* ([new-room (send (gw->current-room world) use-exit 'right)]
             [new-background (bg-room-background (send new-room get-width))])
        (make-game-world
         new-room
         new-background
         (make-coord-pair 50 227)
         'Right
         (gw->player-frame-index world)))
      ;; Move player right
      (make-game-world
       (gw->current-room world)
       (gw->background world)
       (make-coord-pair
        (+ (coord-pair->x (gw->player-coords world)) distance)
        (coord-pair->y (gw->player-coords world)))
       'Right
       (gw->player-frame-index world))))

;; Consume a world and create a world
;; with the next player sprite frame
(define (gw->update-frame-index world)
  (make-game-world
   (gw->current-room world)
   (gw->background world)
   (make-coord-pair
    (coord-pair->x (gw->player-coords world))
    (coord-pair->y (gw->player-coords world)))
    (gw->player-facing world)
   (modulo (+ (gw->player-frame-index world) 1) 1)))

;;;;;;;;;;;; Asset Loading ;;;;;;;;;;;;;;

;; Create a list of frames of right facing character
(define cloaked-figure-R (dict-ref anim-table "Character Facing Right"))

;; Create a list of frames of left facing character

(define cloaked-figure-L (dict-ref anim-table "Character Facing Left"))

;; Create the starting room (always defaults to one-by-one)
(define start-room (new room-1D% [entrance 0]))

;; Load the background
(define background (bg-room-background 1))

;; Take a world and get the player's direction and frame
(define (get-cloaked-figure-bmp direction frame-index)
  (cond ((eq? direction 'Right) (list-ref cloaked-figure-R frame-index))
        ((eq? direction 'Left) (list-ref cloaked-figure-L frame-index))
        (else (empty-scene 256 256))))


;; Display the character over the background at the
;; coords specified in the world
(define (create-cloaked-figure-scene world)
  (crop
   ;; x coordinate of where the crop rectangle begins
   (let ([x (- (coord-pair->x (gw->player-coords world))
               (/ (image-width background) 2))]
         [furthest-left 0]
         [furthest-right (- (image-width (gw->background world))
                            (image-width background) 2)])
         (cond ((< x furthest-left) furthest-left)
               ((> x furthest-right) furthest-right)
               (else x)))
   ;; y coordinate
   0
   ;; the following are based on the size of the first room (they are the window dimensions)
   (image-width background) (image-height background)
   ;; the image itself
   (place-image
    (get-cloaked-figure-bmp (gw->player-facing world)
                            (gw->player-frame-index world))
    (coord-pair->x (gw->player-coords world))
    (coord-pair->y (gw->player-coords world))
    (gw->background world))))

;;;;;;;;;; Game ;;;;;;;;;;;;

;; Make the game world
(define game-world
 (make-game-world
  start-room
  background
  (make-coord-pair 50 227)  ;; y = (- screen-height (/ character-height 2) 8), here (- 320 85 8)
  'Right
  0))

(define run-w/o-audio
  ;; this makes the game load significantly faster
  (lambda ()
    (display "Move left and right with the arrow keys.")
    (newline)
    (big-bang
     game-world
     (on-tick gw->update-frame-index)
     (to-draw create-cloaked-figure-scene)
     (on-key (lambda (w a-key)
               (cond ((key=? a-key "right")
                      (gw->move-player-right w 5))
                     ((key=? a-key "left") (gw->move-player-left w 5))
                     (else w)))))
    (void)))

;; Run the game
(define run-game
  (lambda ()
    (play-audio)
    (newline)
    (display "Move left and right with the arrow keys.")
    (newline)
    (big-bang
     game-world
     (on-tick gw->update-frame-index)
     (to-draw create-cloaked-figure-scene)
     (on-key (lambda (w a-key)
               (cond ((key=? a-key "right")
                      (gw->move-player-right w 5))
                     ((key=? a-key "left") (gw->move-player-left w 5))
                     (else w)))))
    (stop-audio)))
