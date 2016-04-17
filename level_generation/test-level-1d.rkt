#lang racket

;;;; File Name: test-level-1d.rkt
;;;; Author: Wesley Nuzzo
;;;;
;;;; Description:
;;;; This is a test file for the class and functions
;;;; defined in gen-level-1d.rkt.

(require rackunit
         "gen-level-1d.rkt")

(define test-room
  (test-suite
   "Tests for room object"
   (test-case
    "Initializer test"
    (define room (new room-1D% [entrance 0]))
    (check-equal? (send room get-all-rooms) (list room))
    (check-equal? (send room side->wall 'left) 0)
    (check-equal? (send room side->wall 'right) 1))
   (test-case
    "Tests for using exits"
    (define room1 (new room-1D% [entrance 0]))
    (define room2 (send room1 use-exit 'right))
    (check-equal? (send room1 use-exit 'right) room2)
    (check-equal? (send room2 use-exit 'left) room1)

    (check < (send room1 side->wall 'left) (send room2 side->wall 'left))
    (check < (send room1 side->wall 'right) (send room2 side->wall 'right))
    (check-equal? (send room1 side->wall 'right) (send room2 side->wall 'left))

    (check-exn exn:fail? (lambda ()
                           (send room1 use-exit 'left))))))

(define test-map-ascii
  (test-suite
   "Tests for the map-displaying function"
   (test-case
    "Basic test"
    (define room (new room-1D% [entrance 0]))
    (check-equal? (map-ascii room 4) "[-  \n")
    (set! room (send room use-exit 'right))
    (check-equal? (map-ascii room 2) "|-\n")
    (set! room (send room use-exit 'right))
    (check-equal? (map-ascii room 2) "|+\n"))))

(require rackunit/text-ui)
 
(run-tests test-room)
(run-tests test-map-ascii)