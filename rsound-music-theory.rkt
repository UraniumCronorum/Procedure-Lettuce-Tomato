;; Copyright (C) David Benoit 2016
;;
;; Author: David Benoit
;;
;; File: rsound-music-theory.rkt
;;
;; Description: A Programmable Music Theory 
;;              Interface for Racket's rsound 
;;              library. 
;;
;; ToDo:
;;       1) Staff Parts should be broken down into measures
;;       2) Easy instrument creation interface

#lang racket
(require rsound)
(require rsound/piano-tones)
(require rsound/single-cycle)

;; Provide procedures
(provide (all-defined-out))

;;;;;;;  BASIC NON MUSICAL UTILITIES ;;;;;;;;;;

(define (attach-tag tag item)
  (cons tag item))

(define (get-tag object)
  (if (pair? object)
      (car object)
      'unknown))
(define (get-item object)
  (if (pair? object)
      (cdr object)
      'error))

(define (do-x-times proc x)
  (cond ((< x 2) (proc))
        (else
         (do-times proc (- x 1))
         (proc))))

;; Take a note letter name and convert it
;; to its relative midi representation 0-11
(define (note-letter-to-base-number letter)
  (cond ((eq? letter 'C) 0)
        ((eq? letter 'C#) 1)
        ((eq? letter 'D) 2)
        ((eq? letter 'D#) 3)
        ((eq? letter 'Eb) 3)
        ((eq? letter 'E) 4)
        ((eq? letter 'F) 5)
        ((eq? letter 'F#) 6)
        ((eq? letter 'Gb) 6)
        ((eq? letter 'G) 7)
        ((eq? letter 'G#) 8)
        ((eq? letter 'Ab) 8)
        ((eq? letter 'A) 9)
        ((eq? letter 'A#) 10)
        ((eq? letter 'Bb) 10)
        ((eq? letter 'B) 11)
        (else -1)))

(define (midi-number->letter number)
  (define num (remainder number 12))
  (cond ((eq? num 0) 'C)
        ((eq? num 1)'C#)
        ((eq? num 2) 'D)
        ((eq? num 3)'D#)
        ((eq? num 3)'Eb)
        ((eq? num 4) 'E)
        ((eq? num 5) 'F)
        ((eq? num 6)'F#)
        ((eq? num 6)'Gb)
        ((eq? num 7) 'G)
        ((eq? num 8)'G#)
        ((eq? num 8)'Ab)
        ((eq? num 9) 'A)
        ((eq? num 10)'A#)
        ((eq? num 10)'Bb)
        ((eq? num 11) 'B)
        (else -1)))

(define (midi-number->octave num)
  (quotient num 12))

;; Take a note and octave and convert it to
;; its exact midi number
(define (letter-and-octave-to-midi letter octave)
  (+ (* octave 12) (note-letter-to-base-number letter)))

;; The default whole note length is the
;; length of an RSound piano tone
(define default-whole-note-length 132300)

(define (set-global-whole-note-length frames)
  (set! default-whole-note-length frames))

;; Define the length  of a whole note in frames
(define (whole-note-length)
  default-whole-note-length)

;; Take an rsound and create a note that has the
;; duration of a whole note
(define (whole-note note)
  (clip note 0 whole-note-length))

;; Return 0.5 the value of whole-note-length
(define (half-note-length)
  (round (* (whole-note-length) 0.5)))

;; Take an rsound and create a note that has the
;; duration of a half note
(define (clip-to-half-note rsnd)
  (clip rsnd 0 (half-note-length)))

;; Return 0.75 the value of whole-note-length
(define (dotted-half-note-length)
  (round (* (whole-note-length) 0.75)))

;; Take an rsound and create a note that has the
;; duration of a dotted half note
(define (clip-to-dotted-half-note rsnd)
  (clip rsnd 0 (dotted-half-note-length)))


;; Return 0.25 the value of whole-note-length
(define (quarter-note-length)
  (round (* (whole-note-length) 0.25)))

;; Take an rsound and create a note that has the
;; duration of a quarter note
(define (clip-to-quarter-note rsnd)  
  (clip rsnd 0 (quarter-note-length)))

;; Return 0.375 the value of whole-note-length
(define (dotted-quarter-note-length)
  (round (* (whole-note-length) 0.375)))

;; Take an rsound and create a note that has the
;; duration of a dotted quarter note
(define (clip-to-dotted-quarter-note rsnd)
  (clip rsnd 0 (dotted-quarter-note-length)))

;; Return 0.4375 the value of whole-note-length
(define (double-dotted-quarter-note-length)
  (round (* (whole-note-length) 0.4375)))

;; Take an rsound and create a note that has the
;; duration of a double dotted quarter note
(define (clip-to-double-dotted-quarter-note rsnd)
  (clip rsnd 0 (double-dotted-quarter-note-length)))

;; Return 0.125 the value of whole-note-length
(define (eighth-note-length)
  (round (* (whole-note-length) 0.125)))

;; Take an rsound and create a note that has the
;; duration of an eighth note
(define (clip-to-eighth-note rsnd)
  (clip rsnd 0 (eighth-note-length)))

;; Return 0.1875 the value of whole-note-length
(define (dotted-eighth-note-length)
  (round (* (whole-note-length) 0.1875)))

;; Take an rsound and create a note that has the
;; duration of a dotted eighth note
(define (clip-to-dotted-eighth-note rsnd)   
  (clip rsnd 0 (dotted-eighth-note-length)))

;; Return 0.0625 the value of whole-note-length
(define (sixteenth-note-length)
  (round (* (whole-note-length) 0.0625)))

;; Take an rsound and create a note that has the
;; duration of a sixteenth note
(define (clip-to-sixteenth-note rsnd)
  (clip rsnd 0 (sixteenth-note-length)))

;; Return 0.09375 the value of whole-note-length
(define (dotted-sixteenth-note-length)
  (round (* (whole-note-length) 0.09375)))

;; Take an rsound and create a note that has the
;; duration of a dotted sixteenth note
(define (clip-to-dotted-sixteenth-note rsnd)
  (clip rsnd 0 (dotted-sixteenth-note-length)))

;; Return 0.03125 the value of whole-note-length
(define (thirtysecond-note-length)
  (round (* (whole-note-length) 0.03125)))

;; Take an rsound and create a note that has the
;; duration of a thirtysecond (1/32) note
(define (clip-to-thirtysecond-note rsnd)
  (clip rsnd 0 (thirtysecond-note-length)))

(define (letter-and-octave-to-freq letter octave)
  (midi-note-num->pitch (letter-and-octave-to-midi letter octave)))

(define (subdivide beat subdivision)
  (build-list subdivision (lambda (x) (round (/ beat subdivision)))))

(define (make-note-from-midi-num num length)
  (make-note (midi-number->letter num)
             (midi-number->octave num)
             length))

;;;;;;;;;;;; NOTE OBJECT ;;;;;;;;;;;;;;

(define (make-note letter octave duration)
  (attach-tag 'Note (list letter octave duration)))

(define (note? x)
  (eq? (get-tag x) 'Note))

(define (note->letter note)
  (if (note? note)
      (car (get-item note))
      (raise-type-error ' note->letter "Note" note)))

(define (note->octave note)
  (if (note? note)
      (cadr (get-item note))
      (raise-type-error 'note->octave "Note" note )))

(define (note->duration note)
  (if (note? note)
      (caddr (get-item note))
      (raise-type-error 'note->duration "Note" note )))

(define (note->midi-number note)
  (if (note? note)
      (letter-and-octave-to-midi (note->letter note) (note->octave note))
      (raise-type-error 'note->midi-number "Note" note )))

(define (note->freq note)
  (if (note? note)
      (letter-and-octave-to-freq (note->letter note) (note->octave note))
      (raise-type-error 'note->freq "Note" note)))


(define (note->rsound note note-to-rsound-proc)
  (cond ((and (note? note) (procedure? note-to-rsound-proc))
           (note-to-rsound-proc note))
         (else
          (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "Note" note)
             ((raise-type-error 'arg2 "procedure" note-to-rsound-proc))))))




;;;;;;;;;;; REST OBJECT ;;;;;;;;;;;;;;

(define (make-rest duration)
  (attach-tag 'rest duration))

(define (rest? x)
  (eq? (get-tag x) 'rest))

(define (rest->duration rest)
  (if (rest? rest)
      (get-item rest)
      (raise-type-error 'rest->duration "Rest" rest )))


;;;;;;;;;;;; MEASURE OBJECT ;;;;;;;;;;;;

(define (make-measure notelist)
  (attach-tag
   'Measure (filter
            (lambda (x) (or (note? x) (rest? x)))
            notelist)))

(define (measure? x)
  (eq? (get-tag x) 'Measure))

(define (measure->notelist measure)
  (if (measure? measure)
      (get-item measure)
      (raise-type-error 'measure->notelist "Measure" measure)))

(define (measure->rsound measure note-to-rsound-proc)
  (cond ((and (measure? measure) (procedure? note-to-rsound-proc))
           (rs-append* (map note-to-rsound-proc (measure->notelist measure))))
         (else
          (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "Measure" measure)
             (raise-type-error 'arg2 "procedure" note-to-rsound-proc)))))

(define (append-measure m1 m2)
  (cond ((and (measure? m1) (measure? m2))
         (make-measure (append (measure->notelist m1)
                              (measure->notelist m2))))
         (else
           (if (measure? m1)
             (raise-type-error 'arg1 "Measure" m1)
             (raise-type-error 'arg2 "Measure" m1)))))

                

(define (append-measure* measurelist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (append-measure x y))) '() measurelist))


;;;;;;;;;;;;; INSTRUMENT OBJECT ;;;;;;;;;;;;;;

(define (make-instrument name note-to-rsound-proc)
  (cond ((and (procedure? note-to-rsound-proc) (symbol? name))
         (attach-tag 'Instrument (cons name note-to-rsound-proc)))
        (else
         (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "symbol" name)
             ((raise-type-error 'arg2 "procedure" note-to-rsound-proc))))))

(define (instrument? x)
  (eq? (get-tag x) 'Instrument))

(define (instrument->name instrument)
  (if (instrument? instrument)
      (car (get-item instrument))
      (raise-type-error 'instrument->name "Instrument" instrument)))

(define (instrument-proc-wrapper instrument-proc)
  (lambda (note)
    (if (rest? note)
        (silence (rest->duration note))
        (instrument-proc note))))
        
(define (instrument->proc instrument)
  (if (instrument? instrument)
      (instrument-proc-wrapper
       (cdr (get-item instrument)))
      (raise-type-error 'instrument->proc "Instrument" instrument)))


;;;;;;;;;;;;;; STAFF PART OBJECT ;;;;;;;;;;;;;;;;;

(define (make-staff-part instrument measure)
  (cond ((and (instrument? instrument) (measure? measure))
         (attach-tag 'StaffPart (cons instrument measure)))
        (else
         (if (procedure? instrument)
             (raise-type-error 'arg2 "Measure" measure)
             ((raise-type-error 'arg1 "procedure" instrument))))))

(define (staff-part? x)
  (eq? (get-tag x) 'StaffPart))

(define (staff-part->instrument staff-part)
  (if (staff-part? staff-part)
      (car (get-item staff-part))
      (raise-type-error 'staff-part->instrument "StaffPart" staff-part)))

(define (staff-part->measure staff-part)
  (if (staff-part? staff-part)
      (cdr (get-item staff-part))
      (raise-type-error 'staff-part->instrument "StaffPart" staff-part)))

(define (staff-part->rsound staff-part)
 (if (staff-part? staff-part)
     (measure->rsound
      (staff-part->measure staff-part)
      (instrument->proc (staff-part->instrument staff-part)))
     (raise-type-error 'staff-part->instrument "StaffPart" staff-part)))

(define (append-staff-part part1 part2)
    (cond ((and (staff-part? part1) (staff-part? part2))
         (make-staff-part (staff-part->instrument part1)
                          (append-measure (staff-part->measure part1)
                                         (staff-part->measure part2))))
         (else
           (if (staff-part? part1)
             (raise-type-error 'arg1 "StaffPart" part2)
             (raise-type-error 'arg2 "StaffPart" part1)))))

(define (append-staff-part* staff-partlist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (append-staff-part x y))) '() staff-partlist))

;;;;;;;;;;;;;; ENSEMBLE STAFF OBJECT ;;;;;;;;;;;;;;;;

(define (make-ensemble-staff partlist)
  (attach-tag
   'EnsembleStaff (filter (lambda (x) (staff-part? x)) partlist)))

(define (e-staff? x)
  (eq? (get-tag x) 'EnsembleStaff))

(define (e-staff->partlist staff)
  (if (e-staff? staff)
      (get-item staff)
      (raise-type-error 'e-staff->instrument "EnsembleStaff" staff)))

(define (e-staff->rsound staff)
  (rs-overlay* (map staff-part->rsound (e-staff->partlist staff))))

(define (append-ensemble-staff e-staff1 e-staff2)
    (cond ((and (e-staff? e-staff1) (e-staff? e-staff2))
         (make-ensemble-staff
           (map (lambda (x y) (append-staff-part x y))
                (e-staff->partlist e-staff1)
                (e-staff->partlist e-staff2))))
         (else
           (if (e-staff? e-staff1)
             (raise-type-error 'arg1 "EnsembleStaff" e-staff2)
             (raise-type-error 'arg2 "EnsembleStaff" e-staff1)))))

(define (append-ensemble-staff* e-stafflist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (append-ensemble-staff x y))) '() e-stafflist))




;;;;;;;;;; SYNTHESIZERS ;;;;;;;;;;;;


(define (soft-synth-rsound note)
  (cond ((rest? note)
         (silence (rest->duration note)))
        ((note? note)
         (synth-note "vgame" 76 (note->midi-number note) (note->duration note)))))

(define soft-synth (make-instrument 'SoftSynth soft-synth-rsound))
;;;;;;; OTHER NOTES ;;;;;;;;;

;; vgame sounds
;; 1) brass
;; 2) brass
;; 3) sin
;; 4) sin
;; 5) sin
;; 6) brass/saw
;; 7) brass/saw
;; 8) brass
;; 9) brass/woodwind
;; 10) animal crossing*
;; 11)