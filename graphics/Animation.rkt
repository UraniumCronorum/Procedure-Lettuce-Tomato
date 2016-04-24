#lang racket

(require 2htdp/universe)
(require 2htdp/image)

;; This animation was intended to depict the main character's cloak swaying back and forth.
;; It probably will not be used.
;; I mean, just look at it.

(define (animation-test)
  (let ([frames (list (bitmap "Main Character-Standing-0.png")
                      (bitmap "Main Character-Standing-1.png")
                      (bitmap "Main Character-Standing-2.png")
                      (bitmap "Main Character-Standing-3.png"))])
    (animate (lambda (i) (list-ref frames (modulo (floor (/ i 16)) (length frames)))))))