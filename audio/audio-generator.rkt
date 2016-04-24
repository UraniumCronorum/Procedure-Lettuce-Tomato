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

(provide (all-defined-out))
;; Take the length of the desired beat structure
;; to be created in frames.  Subdivide it randomly 
;; but at least twice into powers of two.
(define (gen-random-beat-structure length)
  (define (gen-beat-helper len recursion-depth force)
    (let ([recurse (eq? (random 1 50) 2)]
          [subdivision (subdivide len 2)])
      (cond ((eq? recursion-depth 5) (list len))
            ((eq? recursion-depth 1)
             (append
                      (gen-beat-helper (car subdivision) (+ recursion-depth 1) #f)
                      (gen-beat-helper (cadr subdivision) (+ recursion-depth 1) #f)))
            (force
             (append
                      (gen-beat-helper (car subdivision) (+ recursion-depth 1) #f)
                      (gen-beat-helper (cadr subdivision) (+ recursion-depth 1) #f)))
            ((not recurse) (append
                      (gen-beat-helper (car subdivision) (+ recursion-depth 1) #f)
                      (gen-beat-helper (cadr subdivision) (+ recursion-depth 1) #f)))
            (else
             (list len)))))
  (gen-beat-helper length 0 #t))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (random-pentatonic-note root octaves length)
 (define new-note
   (make-note (note->letter root) (note->octave root) length))
 (define (index-to-pentatonic-note index)
   (cond ((eq? index 1) new-note)
         ((eq? index 2) (note->interval-up new-note 'Minor3rd))
         ((eq? index 3) (note->interval-up new-note 'Perfect4th))
         ((eq? index 4) (note->interval-up new-note 'Perfect5th))
         ((eq? index 5) (note->interval-up new-note 'Minor7th))
         (else
          (error 'InvalidIndex))))        
  (let ((note-index (random 1 6))
        (octave-index (random 1 (+ octaves 1))))
    (cond ((> octave-index 1) (random-pentatonic-note
                               (note->interval-up new-note 'PerfectOctave)
                               (- octaves 1)
                               length))
          (else
           (index-to-pentatonic-note note-index)))))

(define (gen-random-pentatonic-measure root octaves length)
    (let ([beat-structure (gen-random-beat-structure length)])
      (make-measure
        (map (lambda (x) (random-pentatonic-note root octaves x)) beat-structure))))
        
(define (random-harmony-tone harmony low-octave octaves length)
  (let ((note-index (random 1 (+ (harmony->size harmony) 1)))
        (octave-index (random 1 (+ octaves 1))))
    (define new-temp (list-ref (harmony->notelist harmony) (- note-index 1)))
    (define new-note
      (make-note (note->letter new-temp) low-octave length))
    (cond ((> octave-index 1) (random-harmony-tone
                               harmony
                               (+ low-octave 1)
                               (- octaves 1)
                               length))
          (else new-note))))

(define (gen-random-harmony-measure harmony low-octave octaves length)
    (let ([beat-structure (gen-random-beat-structure length)])
      (make-measure
        (map (lambda (x) (random-harmony-tone harmony low-octave octaves x)) beat-structure))))
                   
;;;;;;;;;;;;;;; TEST PLAYGROUND ;;;;;;;;;;;;;;;

;; Create a higher randomly generated
;; atonal staff part that is 15
;; measures long
(define atonal-soprano
  (make-staff-part soft-synth
   (build-list 15 (lambda (x)
                    (gen-random-measure 'C 5 'C 6)))))

;; Create a lower randomly generated
;; atonal staff part that is 15
;; measures long
(define atonal-bass
  (make-staff-part soft-synth
   (build-list 15 (lambda (x) (gen-random-measure 'G 3 'G 4)))))

;; Create an ensemble-staff consisting
;  of the two parts
(define atonal-duet
  (make-ensemble-staff
   (list atonal-soprano atonal-bass)))

;; Build the ensemble-staff into an rsound and play it
;(play (e-staff->rsound atonal-duet))
(define p
  (make-staff-part soft-synth
   (build-list 15 (lambda (x) (gen-random-pentatonic-measure
                   (make-note 'A 4 (whole-note-length)) 2
                   (whole-note-length))))))
(define q
  (make-staff-part soft-synth
   (build-list 15 (lambda (x) (gen-random-pentatonic-measure
                   (make-note 'A 2 (whole-note-length)) 2
                   (whole-note-length))))))

(define I
  (make-harmony
    (list
     (make-note 'C 3 (whole-note-length))
     (make-note 'Eb 3 (whole-note-length))
     (make-note 'G 3 (whole-note-length)))))

(define II-half-dim
  (make-harmony
    (list
     (make-note 'D 3 (whole-note-length))
     (make-note 'F 3 (whole-note-length))
     (make-note 'Ab 3 (whole-note-length))
     (make-note 'C 3 (whole-note-length)))))

(define IV
  (make-harmony
    (list
     (make-note 'F 3 (whole-note-length))
     (make-note 'Ab 3 (whole-note-length))
     (make-note 'C 3 (whole-note-length)))))

(define V7
  (make-harmony
    (list
     (make-note 'G 3 (whole-note-length))
     (make-note 'B 3 (whole-note-length))
     (make-note 'D 3 (whole-note-length))
     (make-note 'F 3 (whole-note-length)))))

(define harmony-part1
  (make-staff-part soft-synth
   (reverse
    (list
     (gen-random-harmony-measure I 5 1 (whole-note-length))
     (gen-random-harmony-measure IV 5 1 (whole-note-length))     
     (gen-random-harmony-measure II-half-dim 5 1 (whole-note-length))
     (gen-random-harmony-measure V7 5 1 (whole-note-length))))))

(define harmony-part2
  (make-staff-part soft-synth
   (reverse
    (list
     (gen-random-harmony-measure I 3 1 (whole-note-length))
     (gen-random-harmony-measure IV 3 1 (whole-note-length))
     (gen-random-harmony-measure II-half-dim 3 1 (whole-note-length))
     (gen-random-harmony-measure V7 3 1 (whole-note-length))))))

(define harmony-part3
  (make-staff-part soft-synth
   (reverse
    (list
     (gen-random-harmony-measure I 4 1 (whole-note-length))
     (gen-random-harmony-measure IV 4 1 (whole-note-length))     
     (gen-random-harmony-measure II-half-dim 4 1 (whole-note-length))
     (gen-random-harmony-measure V7 4 1 (whole-note-length))))))

(define harmony-duet
  (make-ensemble-staff
   (list (append-staff-part harmony-part1 harmony-part1)
         (append-staff-part harmony-part3 harmony-part3)
         (append-staff-part harmony-part2 harmony-part2))))

             
(define pentatonic-duet
  (make-ensemble-staff (list p q)))

(play (e-staff->rsound harmony-duet))

;; Print some info
(display "This racket program requires the package rsound (available via raco).")
(newline)
(display "It should be run from the same directory that contains synthesis-framework.rkt")
(newline)
