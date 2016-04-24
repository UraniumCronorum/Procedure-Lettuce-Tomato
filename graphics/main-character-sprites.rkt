#lang racket/gui

;;;; File Name: main-character-sprites.rkt
;;;; Author: Wesley Nuzzo
;;;;
;;;; Description:
;;;; This file currently contains the definition of single sprite frame.
;;;; Think of it as a demonstration of how the sprite-frame% class works.
;;;;
;;;; The intention is for this file, when finished, to contain the instructions
;;;; necessary for creating all of the main character's sprite frames.

(require "./sprites.rkt")

;;; Main ;;;

(define (display-points points)
  (for ([point points])
    (printf "(~a, ~a)\n"
            (send point get-x)
            (send point get-y))))

(define pallet null)

(define my-sprite
  (let ((temp (new sprite-frame%
                  [_width 256]
                  [_height 256])))

    (define hood-points
      (all-points (make-object point% 128 0)
                  (map list
                       '(0 -1/2 -1 -2 -3 -4 0 2 1/2 0 1 4 0 -4 -3 -2 -1)
                       '(l l l l l l d r r r l l u r r r r)
                       '(8 3 3 3 2 2 2 1 1 19 2 2 4 1 2 3 1))))

    (define shadow-points
      (all-points (last hood-points)
                  (map list
                       '(-1 -2 -3 -4 0 4 1 -1 -2 -3 -4 0 4 1)
                       '(l l l l d r r r r r r u l l)
                       '(1 3 2 1 4 2 2 1 3 2 1 4 2 2))))


    (define cloak-points
      (all-points (list-ref hood-points 9)
                  (map list
                       '(-1 -2 -3 -4 -5 -6 -5 -4 -3 -2 -1 -1/2 0
                            2 3 4 10 4 3 2 1 0)
                       '(l l l l l l l l l l l l r
                           l l l l l l l l l)
                       '(4 2 2 2 6 8 6 4 4 3 2 1 100
                           4 4 10 7 4 4 4 1 19))))

    (send temp add-path
          "hood"
          (multiple-line-path hood-points)
          #:brush (new brush% [color "dark gray"]))
    
    (send temp add-path
          "shadow"
          (multiple-line-path shadow-points)
          #:brush (new brush% [color "black"]))

    (send temp add-path
          "cloak"
          (multiple-line-path cloak-points)
          #:brush (new brush% [color "dark gray"]))
    
    (lambda (request)
      (cond
        ((eq? request 'display) (send temp display-window))
        ((eq? request 'save) (send temp write-file))
        ((eq? request 'points) (display-points hood-points)
                               (display #\newline)
                               (display-points shadow-points)
                               (display #\newline)
                               (display-points cloak-points))
        (else (error "Invalid request."))))))

