#lang racket

;;;; File Name: gen-level-1d.rkt
;;;; Author: Wesley Nuzzo
;;;;
;;;; Description:
;;;; This module provides the definintion of the class room-1D%, which
;;;; represents a one-dimensional room. The room, when created, sets its
;;;; own dimensions and exits, and when you use an exit that doesn't have
;;;; a room on the other side, a new room will be created for that location.
;;;;
;;;; Running this file will enter the (main-loop) function, which creates a starting
;;;; room and then enters a loop where the user enters "left" or "right" to exit
;;;; the current room through the appropriate exit, or "q" to end the loop.
;;;; On each iteration of the loop, an ascii-art map is displayed.
;;;; Key:
;;;; exits: "-" (current room) and "+"
;;;; walls: "[" and "]" (current room) and "|"

(provide (all-defined-out))

;;;; Author: Wesley Nuzzo

(define room-1D%
  (let ((all-rooms '())
        (max-depth 4))
    (class object%
      
      ;; initializer
      (init entrance [direction #f])

      (if direction
          (set! all-rooms (cons this all-rooms))
          (set! all-rooms (list this)))
      
      (define exits '())
      (define left entrance)
      (define right entrance)

      (set-dimensions direction)
      (set-exits)

      (super-new)

      ;; accessors
      (define/public (get-exits)
        exits)

      (define/public (get-all-rooms)
        all-rooms)

      (define/public (get-width)
        (- right left))

      (define/public (get-height)
        1)

      (define/public (side->wall side)
        (cond ((eq? side 'left) left)
              ((eq? side 'right) right)
              (else "object-1D%: invalid side.")))

      (define/public (check-exit e)
        (memq e exits))

      (define/public (position->side e)
        (cond ((= left e) 'left)
              ((= right e) 'right)
              (else (error "room-1D%: given position not on wall"))))

      ;;; Helpers for initializer ;;;

      ;; set the positions of the walls for a new room
      (define/private (set-dimensions direction)
        (cond ((not direction) (set! right (+ right 1)))
              ((eq? direction 'right) (set! right
                                            (random (+ right 1)
                                                    (+ (furthest-position 'right) 1))))
              ((eq? direction 'left) (set! left
                                           (random (furthest-position 'left)
                                                   left)))
              (else (error "room-1D%: invalid direction for set-dimensions"))))

      ;; add the positions of the exits for a new room
      (define/private (set-exits)
        (for-each (lambda (e)
                    (if (nor (check-exit e) (= e 0))
                             (set! exits (cons e exits))
                             (void)))
                  (list left right)))

      ;; return the position of the nearest room, or the position max-depth away, or 0.
      ;; whichever is closer
      (define/public (furthest-position direction)
        (let ((near-wall (if (eq? direction 'left)
                             left
                             right))
              (correct-direction? (if (eq? direction 'left)
                                      negative?
                                      positive?))
              (accumulator (if (eq? direction 'left)
                               max
                               min))
              (farthest-allowable (if (eq? direction 'left)
                                      (- max-depth)
                                      max-depth))
              (map-bound (if (eq? direction 'left)
                             '(0)
                             '())))
          (apply accumulator
                 (append map-bound
                         (map (lambda (x)
                                (+ x near-wall))
                              (append (list farthest-allowable)
                                      (filter correct-direction?
                                              (map (lambda (room)
                                                     (send room distance-from near-wall))
                                                   all-rooms))))))))

      ;; return the distance from the nearest wall to the position
      ;; distance returned may be negative
      (define/public (distance-from position)
        (let ((left-distance (- left position))
              (right-distance (- right position)))
          (if (< (+ left-distance right-distance)
                 0)
              left-distance
              right-distance)))

      ;;; Exit Room ;;;
      
      ;; return the room on the other side, or create it if it doesn't exist
      (define/public (use-exit e)
        (if (memq e '(left right))
            (let* ((p (side->wall e))
                   (destination (findf (lambda (room)
                                        (send room check-exit p))
                                      (filter (lambda (room)
                                                (not (eq? room this)))
                                              all-rooms))))
              (if (memq p exits)
                  (if destination
                      destination
                      (new room-1D% [entrance p] [direction e]))
                  (error "room-1D%: No exit on given side")))
            (error "room-1D%: Invalid input to use-exit"))))))

;; Display an ascii art map
(define (map-ascii room size)
  (define rooms (send room get-all-rooms))
  (define walls (foldl (lambda (side out)
                         (append (map (lambda (room)
                                        (send room side->wall side))
                                      rooms)
                                 out))
                       '()
                       '(left right)))
  (define exits (foldl (lambda (room out)
                         (append (send room get-exits)
                                 out))
                       '()
                       rooms))
  ;;(print exits)
  (list->string (append
                 (map (lambda (i)
                        (cond
                          ((memq i (send room get-exits)) #\-)
                          ((eq? i (send room side->wall 'left)) #\[)                          
                          ((eq? i (send room side->wall 'right)) #\])                          
                          ((memv i exits) #\+)
                          ((memv i walls) #\|)
                          (else #\space)))
                      (stream->list (in-range size)))
                 '(#\newline))))

;; loop for inputs of which exits to go through, displaying the map as it changes
(define (main-loop)
  (define start (new room-1D% [entrance 0]))
  (define (helper room)
    (display (map-ascii room 32))
    (define input (read-line))
    
    (if (string=? input "q")
        (void)
        (helper
         (cond ((string=? input "left") (send room use-exit 'left))
                ((string=? input "right") (send room use-exit 'right))
                (else (begin
                        (display "Invalid input\n")
                        room))))))
  (helper start))

(module+ main (main-loop))
