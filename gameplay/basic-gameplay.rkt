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
         player-frame-index
         enemy-coords
         enemy-frame-index
         enemy-facing)
  
  (attach-tag 'GameWorld
              (list room
                    background
                    player-coords
                    player-facing
                    player-frame-index
                    enemy-coords
                    enemy-frame-index
                    enemy-facing)))

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
(define (gw->enemy-coords world)
  (sixth (get-item world)))
(define (gw->enemy-frame-index world)
  (seventh (get-item world)))
(define (gw->enemy-facing world)
  (eighth (get-item world)))


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
         (gw->player-frame-index world)
         (make-coord-pair
          (coord-pair->x (gw->enemy-coords world))
          (coord-pair->y (gw->enemy-coords world)))
         (gw->enemy-frame-index world)
         (gw->enemy-facing world)))
      ;; Move player left
      (make-game-world
       (gw->current-room world)
       (gw->background world)
       (make-coord-pair
        (- (coord-pair->x (gw->player-coords world)) distance)
        (coord-pair->y (gw->player-coords world)))
       'Left
       (gw->player-frame-index world)
   (make-coord-pair
    (coord-pair->x (gw->enemy-coords world))
    (coord-pair->y (gw->enemy-coords world)))
   (gw->enemy-frame-index world)
   (gw->enemy-facing world))))

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
         (gw->player-frame-index world)
         (make-coord-pair
          (coord-pair->x (gw->enemy-coords world))
          (coord-pair->y (gw->enemy-coords world)))
         (gw->enemy-frame-index world)
         (gw->enemy-facing world)))
      ;; Move player right
      (make-game-world
       (gw->current-room world)
       (gw->background world)
       (make-coord-pair
        (+ (coord-pair->x (gw->player-coords world)) distance)
        (coord-pair->y (gw->player-coords world)))
       'Right
       (gw->player-frame-index world)
   (make-coord-pair
    (coord-pair->x (gw->enemy-coords world))
    (coord-pair->y (gw->enemy-coords world)))
   (gw->enemy-frame-index world)
   (gw->enemy-facing world))))

;; Consume a world and create a world
;; with the player attacking right
(define (gw->player-attack world)
  (if (>= (coord-pair->x (gw->player-coords world)) 640)
      world  
      (make-game-world
       (gw->current-room world)
       (gw->background world)
       (make-coord-pair
        (coord-pair->x (gw->player-coords world))
        (coord-pair->y (gw->player-coords world)))
       (cond ((eq? (gw->player-facing world) 'Right) 'AttackRight)
             ((eq? (gw->player-facing world) 'Left) 'AttackLeft)
             (else (gw->player-facing world)))
       (gw->player-frame-index world)
   (make-coord-pair
    (coord-pair->x (gw->enemy-coords world))
    (coord-pair->y (gw->enemy-coords world)))
   (gw->enemy-frame-index world)
   (gw->enemy-facing world)
       )))

;; Consume a world and create a world
;; with the next player sprite frame
(define (gw->update-frame-index world)
  (make-game-world
   (gw->current-room world)
   (gw->background world)
   (make-coord-pair
    (coord-pair->x (gw->player-coords world))
    (coord-pair->y (gw->player-coords world)))
   (cond ((eq? (gw->player-facing world) 'AttackRight) 'Right)
         ((eq? (gw->player-facing world) 'AttackLeft) 'Left)
         (else
          (gw->player-facing world)))
   (+ (gw->player-frame-index world) 1)

   (make-coord-pair
    ( +(coord-pair->x (gw->enemy-coords world)) (- (random 1 2) 1))
    (coord-pair->y (gw->enemy-coords world)))
   (+ (gw->enemy-frame-index world) 1)
   (gw->enemy-facing world)))


;;;;;;;;;;;; Asset Loading ;;;;;;;;;;;;;;

;; Create a list of frames of right facing character
(define cloaked-figure-R (dict-ref anim-table "Character Facing Right"))
(define cloaked-figure-attack-R (dict-ref anim-table "Character Attack Right"))

;; Create a list of frames of left facing character

(define cloaked-figure-L (dict-ref anim-table "Character Facing Left"))
(define cloaked-figure-attack-L (dict-ref anim-table "Character Attack Left"))

(define enemy-R
 (dict-ref anim-table "Enemy Facing Right"))
(define enemy-L
 (dict-ref anim-table "Enemy Facing Left"))

;; Create the starting room (always defaults to one-by-one)
(define start-room (new room-1D% [entrance 0]))

;; Load the background
(define background (bg-room-background 1))

;; Take a world and get the player's direction and frame
(define (get-cloaked-figure-bmp direction frame-index)
  (cond ((eq? direction 'Right) (list-ref cloaked-figure-R frame-index))
        ((eq? direction 'Left) (list-ref cloaked-figure-L frame-index))
        ((eq? direction 'AttackRight) (list-ref cloaked-figure-attack-R frame-index))
        ((eq? direction 'AttackLeft) (list-ref cloaked-figure-attack-L frame-index))
        (else (empty-scene 256 256))))

(define (get-enemy-bmp direction frame-index)
  (cond ((eq? direction 'Right) (list-ref enemy-R frame-index))
        ((eq? direction 'Left) (list-ref enemy-L frame-index))
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
   (get-enemy-bmp (gw->enemy-facing world)
                  (modulo (floor (/ (gw->enemy-frame-index world) 2))
                          2))
   (coord-pair->x (gw->enemy-coords world))
   (coord-pair->y (gw->enemy-coords world))
     (place-image
      (get-cloaked-figure-bmp (gw->player-facing world)
                            (modulo (gw->player-frame-index world) 1))
      (coord-pair->x (gw->player-coords world))
      (coord-pair->y (gw->player-coords world))
      (gw->background world)))))

;;;;;;;;;; Game ;;;;;;;;;;;;

;; Make the game world
(define game-world
 (make-game-world
  start-room
  background
  (make-coord-pair 50 227)  ;; y = (- screen-height (/ character-height 2) 8), here (- 320 85 8)
  'Right
  0
  (make-coord-pair 100 100)
  0
  'Right))

(define run-w/o-audio
  ;; this makes the game load significantly faster
  (lambda ()
    (display "Move left and right with the arrow keys. Attack with space bar")
    (newline)
    (big-bang
     game-world
     (on-tick gw->update-frame-index)
     (to-draw create-cloaked-figure-scene)
     (on-key (lambda (w a-key)
               (cond ((key=? a-key "right")
                      (gw->move-player-right w 5))
                     ((key=? a-key "left") (gw->move-player-left w 5))
                     ((key=? a-key " ") (gw->player-attack w))
                     (else w))))
     (on-release (lambda (w a-key)
                   (cond ((key=? a-key " ") (gw->player-attack w))
                         (else w)))))
    (void)))

;; Run the game
(define run-game
  (lambda ()
    (play-audio)
    (newline)
    (display "Move left and right with the arrow keys. Attack with space bar")
    (newline)
    (big-bang
     game-world
     (on-tick gw->update-frame-index)
     (to-draw create-cloaked-figure-scene)
     (on-key (lambda (w a-key)
               (cond ((key=? a-key "right")
                      (gw->move-player-right w 5))
                     ((key=? a-key "left") (gw->move-player-left w 5))
                     ((key=? a-key " ") (gw->player-attack w))
                     (else w))))
     (on-release (lambda (w a-key)
                   (cond ((key=? a-key " ") (gw->player-attack w))
                         (else w)))))
    (stop-audio)))
