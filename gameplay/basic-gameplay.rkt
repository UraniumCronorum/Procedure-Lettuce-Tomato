#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "../graphics/Animation.rkt")
(require "../audio/audio-generator.rkt")


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
      'unknown))

;; Untag an object
(define (get-item object)
  (if (pair? object)
      (cdr object)
      'error))

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
         player-coords
         player-facing
         player-frame-index)
  (attach-tag 'GameWorld
              (list 
                    player-coords
                    player-facing
                    player-frame-index)))

;; Check if an item is a game world
(define (game-world? x)
  (eq? (get-tag x) 'GameWorld))

;; Accessors
(define (gw->player-coords world)
  (car (get-item world)))
(define (gw->player-facing world)
  (cadr (get-item world)))
(define (gw->player-frame-index world)
  (caddr (get-item world)))

;; Consume a world and create a world
;; with the player moved left
(define (gw->move-player-left world distance)
  (if (<= (coord-pair->x (gw->player-coords world)) 50)
      world
      (make-game-world
       (make-coord-pair
        (- (coord-pair->x (gw->player-coords world)) distance)
        (coord-pair->y (gw->player-coords world)))
       'Left
       (gw->player-frame-index world))))

;; Consume a world and create a world
;; with the player moved right
(define (gw->move-player-right world distance)
  (if (>= (coord-pair->x (gw->player-coords world)) 1024)
      world  
      (make-game-world
       (make-coord-pair
        (+ (coord-pair->x (gw->player-coords world)) distance)
        (coord-pair->y (gw->player-coords world)))
       'Right
       (gw->player-frame-index world))))

;; Consume a world and create a world
;; with the next player sprite frame
(define (gw->update-frame-index world)
  (make-game-world
   (make-coord-pair
    (coord-pair->x (gw->player-coords world))
    (coord-pair->y (gw->player-coords world)))
    (gw->player-facing world)
   (modulo (+ (gw->player-frame-index world) 1) 4)))

;;;;;;;;;;;; Asset Loading ;;;;;;;;;;;;;;

;; Create a list of frames of right facing character
(define cloaked-figure-R
  (list
   (bitmap "../graphics/Main Character-Standing-0.png")
   (bitmap "../graphics/Main Character-Standing-1.png")
   (bitmap "../graphics/Main Character-Standing-2.png")
   (bitmap "../graphics/Main Character-Standing-3.png")))

;; Create a list of frames of left facing character

(define cloaked-figure-L
  (list
   (flip-horizontal (bitmap "../graphics/Main Character-Standing-0.png"))
   (flip-horizontal (bitmap "../graphics/Main Character-Standing-1.png"))
   (flip-horizontal (bitmap "../graphics/Main Character-Standing-2.png"))
   (flip-horizontal (bitmap "../graphics/Main Character-Standing-3.png"))))

;; Load the background
(define background (bitmap "../graphics/background.jpeg"))

;; Take a world and get the player's direction and frame
(define (get-cloaked-figure-bmp direction frame-index)
  (cond ((eq? direction 'Right) (list-ref cloaked-figure-R frame-index))
        ((eq? direction 'Left) (list-ref cloaked-figure-L frame-index))
        (else (empty-scene 256 256))))

;; Display the character over the background at the
;; coords specified in the world
(define (create-cloaked-figure-scene world)
  (place-image
   (get-cloaked-figure-bmp (gw->player-facing world)                   (gw->player-frame-index world))
   (coord-pair->x (gw->player-coords world))
   (coord-pair->y (gw->player-coords world))
   background))

;;;;;;;;;; Game ;;;;;;;;;;;;

;; Make the game world
(define game-world
 (make-game-world
  (make-coord-pair 100 600)
  'Right
  0))

;; Run the game
(define run-game
  (lambda ()
    (play-audio)
    (newline)
    (display "Move left and right with the arrow keys.")
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
 