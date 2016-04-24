#lang racket/gui

;;;; File Name: sprites.rkt
;;;; Author: Wesley Nuzzo
;;;;
;;;; Description:
;;;; This module provides the class sprite-frame% which stores the information
;;;; for drawing a sprite as a series of paths with configurations for the pen
;;;; and brush. These sprites can be either written to a file or displayed to
;;;; the screen using the appropriate member functions.
;;;; The module also provides an assortment of functions for creating a list of
;;;; points from slope and magnitude information, and for creating new paths
;;;; from these lists of points.

(require racket/dict)

(provide (all-defined-out))

(struct p-data (path pen brush))
(define default-pen (new pen% [color "black"]))
(define default-brush (new brush% [color "white"]))

;;; Classes ;;;
(define sprite-frame%
  (class object%
    
    ;; initializer
    (init _width _height [_paths null] [_name "Untitled"])
    (define width _width)
    (define height _height)
    (define paths _paths)
    (define name _name)
    (super-new)

    ;; modify
    (define/public (add-path name path #:pen [pen default-pen] #:brush [brush default-brush])
      (set! paths (dict-set paths name
                            (p-data path pen brush))))

    ;; basic output function
    (define/public (output-to-dc dc)
      (for ([path (map cdr paths)])
        (send dc set-brush (p-data-brush path))
        (send dc set-pen (p-data-pen path))
        (send dc draw-path (p-data-path path))))

    ;; display
    (define/public (display-window)
      (define frame (new frame%
                         [label name]
                         [width width]
                         [height height]))
      (new canvas% [parent frame]
           [paint-callback
            (lambda (canvas dc)
              (output-to-dc dc))])
      (send frame show #t))

    ;; save
    (define/public (write-file)
      (define filename (string-append name ".png"))
      (define target (make-bitmap width height))
      (define dc (new bitmap-dc% [bitmap target]))

      ;; create image and write to file
      (output-to-dc dc)
      (send target save-file filename 'png))))

(define sprite-animation%
  (class object%

    ;; initializer
    (init [_frames null])
    (define frames _frames)
    (super-new)

    ;; accessors
    (define/public (get-frame frame-number)
      (list-ref frames frame-number))

    ;; output
    (define/public (display-window frame-number)
      (send (list-ref frames frame-number)
            display-window))

    (define/public (write-file frame-number)
      (send (list-ref frames frame-number)
            write-file))))

(define sprite% null)

;;; Functions ;;;

;; Two representations for a line
(define (get-slope-&c start end [horizontal #t])
  (let ((x1 (send start get-x))
        (y1 (send start get-y))
        (x2 (send end get-x))
        (y2 (send end get-y)))
    (let ((∆y (- y2 y1))
          (∆x (- x2 x1)))
      (if horizontal
          ;; return slope, direction, and multiplier
          ;; note the divide by zero error if ∆x=0
          (let* ((slope (/ ∆y ∆x))
                (direction (if (positive? ∆x) 'r 'l))
                (multiplier (/ ∆x (denominator slope))))
            (list slope direction multiplier))
          ;; return slope, direction, and multiplier
          ;; note that positive y is down
          (let* ((slope (/ ∆x ∆y))
                (direction (if (positive? ∆y) 'd 'u))
                (multiplier (/ ∆y (denominator slope))))
            (list slope direction multiplier))))))

(define (end-point start-point slope direction multiplier)
  ;; return the endpoint of a line with the given slope and length
  (let ((x (send start-point get-x))
        (y (send start-point get-y))
        (∆x (* multiplier (denominator slope)))
        (∆y (* multiplier (numerator slope))))
    ;; nonzero slope
    (cond
      ;; trace line to the right
      ((eq? direction 'r)
       (make-object point%
         (+ x ∆x)
         (+ y ∆y)))
      ;; trace line to the left
      ((eq? direction 'l)
       (make-object point%
         (- x ∆x)
         (- y ∆y)))
      ;; trace line up (note that this will, in fact, trace upwards)
      ((eq? direction 'u)
       (make-object point%
         (+ x ∆y)
         (- y ∆x)))
      ;; trace line down
      ((eq? direction 'd)
       (make-object point%
         (- x ∆y)
         (+ y ∆x)))
      (else (error "invalid direction")))))

;; apply above to lists
(define (all-points start-point deriv)
  (foldl (λ (deriv out)
           (let ([slope (first deriv)]
                 [direction (second deriv)]
                 [multiplier (third deriv)])
             (cons (end-point (first out) slope direction multiplier)
                   out)))
           (list start-point)
           deriv))

;; paths

(define (single-line-path start end)
  (define path (new dc-path%))
  (send path move-to
        (send start get-x)
        (send start get-y))
  (send path line-to
        (send end get-x)
        (send end get-y))
  path)

(define (multiple-line-path points)
  (let ((start-point (first points))
        (other-points (rest points)))
    (define path (new dc-path%))
    (send path move-to
          (send start-point get-x)
          (send start-point get-y))
    (send path lines other-points)
    path))

