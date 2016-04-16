;; Copyright (C) David Benoit 2016
;;
;; Author: David Benoit
;;
;; File: random-atonal-generator.rkt
;;
;; Description: Generate random atonal musical
;;              fragments.  
;;
;;

#lang racket
(require rsound)
(require rsound/single-cycle)
(require "synthesis-framework.rkt")

;; Take the length of the desired beat structure
;; to be created in frames.  Subdivide it randomly 
;; into powers of two.
(define (gen-random-beat-structure length)
  (define (gen-beat-helper len recursion-depth)
    (let ([recurse (eq? (random 1 6) 2)]
          [subdivision (subdivide len 2)])
      (cond ((eq? recursion-depth 5) (list len))
            ((not recurse) (append
                      (gen-beat-helper (car subdivision) (+ recursion-depth 1))
                      (gen-beat-helper (cadr subdivision) (+ recursion-depth 1))))
            (else
             (list len)))))
  (gen-beat-helper length 0))

;; Take a length in frames and generate notes
;; between midi numbers min and max
(define (gen-random-notes length letter-min octave-min letter-max octave-max)
  (let ([beat-structure (gen-random-beat-structure length)]
        [midi-min (letter-and-octave-to-midi letter-min octave-min)]
        [midi-max (letter-and-octave-to-midi letter-max octave-max)])
        (map (lambda (x) (make-note-from-midi-num
                          (random midi-min midi-max) x)) beat-structure)))

;; Generate a measure object containing random
;; notes and a random 
(define (gen-random-measure letter-min octave-min letter-max octave-max)
  (make-measure
   (gen-random-notes (whole-note-length) letter-min octave-min letter-max octave-max)))


;;;;;;;;;;;;;;; TEST PLAYGROUND ;;;;;;;;;;;;;;;

;; Create a higher randomly generated
;; atonal staff part that is 15
;; measures long
(define soprano
  (make-staff-part soft-synth
   (build-list 15 (lambda (x)
                    (gen-random-measure 'C 5 'C 6)))))

;; Create a lower randomly generated
;; atonal staff part that is 15
;; measures long
(define bass
  (make-staff-part soft-synth
   (build-list 15 (lambda (x) (gen-random-measure 'G 3 'G 4)))))

;; Create an ensemble-staff consisting
;  of the two parts
(define atonal-duet
  (make-ensemble-staff
   (list soprano bass)))

;; Build the ensemble-staff into an rsound and play it
(play (e-staff->rsound atonal-duet))

;; Print some info
(display "This racket program requires the package rsound (available via raco).")
(newline)
(display "It should be run from the same directory that contains synthesis-framework.rkt")
(newline)
(display "..................................................................")
(newline)
(display "Sound clip is approx. 45 seconds long")
(newline)
(display "Sound is generated randomly with no consideration of tonal harmony.")
(newline)
(display "It will be different every time it is played")
(newline)
(display "Tonal harmony rules will be implemented next.")
