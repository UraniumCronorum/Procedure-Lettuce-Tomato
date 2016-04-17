#lang racket/gui

;;;; File Name: 1D-level-visual.rkt
;;;; Author: Wesley Nuzzo
;;;;
;;;; This file defines a function (visual-map-loop), which
;;;; works similar to the (main-loop) function in gen-level-1d,
;;;; but uses rackets draw library, along with a seperate window,
;;;; to display the map. The current room is highlighted in blue.
;;;;
;;;; To start the loop, execute (visual-map-loop). Note that you
;;;; will need to click on this window to bring it back in focus,
;;;; as racket's gui library doesn't seem to cooperate well with
;;;; the (read-line) function.
;;;;
;;;; The map is brought into focus after every instruction so you
;;;; can see it change.


(require "gen-level-1d.rkt")

(define unit-width 40)
(define room-height 40)
(define exit-inset 6)
(define exit-height (- room-height (* 2 exit-inset)))

(define window-width 800)
(define window-height 300)

;; function for displaying map
(define (display-map room dc)
  (define rooms (send room get-all-rooms))
  (define exits (foldl (lambda (room out)
                         (append (send room get-exits)
                                 out))
                       '()
                       rooms))
  (define you-are-here (new brush% [color "aqua"]))
  (define normal (new brush% [color "white"]))

  (for ([r rooms])
    (if (eq? r room)
        (send dc set-brush you-are-here)
        (send dc set-brush normal))
    (send dc draw-rectangle
          (* unit-width (send r side->wall 'left))
          (/ window-height 3)
          (* unit-width (- (send r side->wall 'right)
                           (send r side->wall 'left)))
          room-height))
  (send dc set-brush normal)
  (for ([exit exits])
    (send dc draw-rectangle
          (- (* unit-width exit) (/ exit-inset 2))
          (+ (/ window-height 3) exit-inset)
          exit-inset
          exit-height)))

;; loop for interacting with/updating map
(define (visual-map-loop)
  (define start (new room-1D% [entrance 0]))
  (define current start)

  (define frame (new frame%
                     [label "Level Map"]
                     [width window-width]
                     [height window-height]))
  (define canvas
    (new canvas% [parent frame]
                 [paint-callback
                  (lambda (canvas dc)
                    (display-map current dc))]))
  
  (send frame show #t)
  (define (helper room)

    (set! current room)
    (send canvas refresh-now)
    (send frame show #t)
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

