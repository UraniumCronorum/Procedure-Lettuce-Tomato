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
(require "tonal-harmony.rkt")

(provide (all-defined-out))

;; Take the length of the desired beat structure
;; to be created in frames.  Subdivide it randomly 
;; but at least twice into powers of two.
(define (gen-random-beat-structure length)
  (define (gen-beat-helper len recursion-depth force)
    (let ([recurse (eq? (random 1 50) 2)]
          [subdivision (subdivide len 2)])
      (cond ((eq? recursion-depth 5) (list len))
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

;; Create a random pentatonic note
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


;; Get a random note from a given harmony and range
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

;; Generate a measure of random notes
;; from a given harmony and range
(define (gen-random-harmony-measure harmony low-octave octaves length)
    (let ([beat-structure (gen-random-beat-structure length)])
      (make-measure
        (map (lambda (x) (random-harmony-tone harmony low-octave octaves x)) beat-structure))))

;; Take a harmonic progression and
;; create a melody in the form of a
;; list of measure objects
(define (build-progression-melody progression low-octave octaves repeats)
 (build-list
   repeats (lambda (x) (append-measure* (reverse
   (map (lambda (x)
          (gen-random-harmony-measure x low-octave octaves (whole-note-length)))
        (progression->chordlist progression)))))))

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
;; of the two parts
(define atonal-duet
  (make-ensemble-staff
   (list atonal-soprano atonal-bass)))

;;;;;;;;;; Pentatonic Duet ;;;;;;;;;
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
(define pentatonic-duet
  (make-ensemble-staff (list p q)))


;;;;;;;;;;; Tonal Trio ;;;;;;;;;;;;;;

(define (random-key)
  (make-note-from-midi-num (- (random 1 12) 1) 0))

(define (random-progression)
  (list-ref
    tonal-proglist
    (- (random 1 (+ (length tonal-proglist) 1)) 1)))

(define (random-harmony-trio)
  (set-whole-note-length
   (* (whole-note-length)
      (/ 120 (random 80 150))))
  (let ((prog (random-progression))
        (synth (random-synth))
        (key (random-key))
        (repeats 6))
        (make-ensemble-staff
         (list
          (make-staff-part
            synth
            (build-progression-melody
             (prog key (whole-note-length))
             3
             1
             repeats))
          (make-staff-part
            synth
            (build-progression-melody
             (prog key (whole-note-length))
             4
             2
             repeats))
          (make-staff-part
            synth
            (build-progression-melody
             (prog key (whole-note-length))
             3
             2
             repeats))
             ))))

(define (play-audio)
  (display "Building generated audio. Game will begin shortly...")
  (newline)
  (play (e-staff->rsound (random-harmony-trio))))

(define (stop-audio)
  (stop))

;; Print some info
(display "This racket program requires the package rsound (available via raco).")
(newline)
