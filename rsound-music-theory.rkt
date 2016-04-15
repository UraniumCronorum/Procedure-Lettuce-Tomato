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
(provide note-letter-to-base-number)
(provide letter-and-octave-to-midi)
(provide whole-note-length)
(provide whole-note-length)
(provide dotted-half-note-length)
(provide half-note-length)
(provide double-dotted-quarter-note-length)
(provide dotted-quarter-note-length)
(provide quarter-note-length)
(provide dotted-eighth-note-length)
(provide eighth-note-length)
(provide dotted-sixteenth-note-length)
(provide sixteenth-note-length)
(provide thirtysecond-note-length)

;;;;;;;  BASIC UTILITIES ;;;;;;;;;;

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


;;;;;;;;;;;; MELODY OBJECT ;;;;;;;;;;;;

(define (make-melody notelist)
  (attach-tag
   'Melody (filter
            (lambda (x) (or (note? x) (rest? x)))
            notelist)))

(define (melody? x)
  (eq? (get-tag x) 'Melody))

(define (melody->notelist melody)
  (if (melody? melody)
      (get-item melody)
      (raise-type-error 'melody->notelist "Melody" melody)))

(define (melody->rsound melody note-to-rsound-proc)
  (cond ((and (melody? melody) (procedure? note-to-rsound-proc))
           (rs-append* (map note-to-rsound-proc (melody->notelist melody))))
         (else
          (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "Melody" melody)
             ((raise-type-error 'arg2 "procedure" note-to-rsound-proc))))))

;; (define (append-melody m1 m2) 1)
;; (define (append-melody* melodylist) 1)


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

(define (make-staff-part instrument melody)
  (cond ((and (instrument? instrument) (melody? melody))
         (attach-tag 'Staff-Part (cons instrument melody)))
        (else
         (if (procedure? instrument)
             (raise-type-error 'arg2 "Melody" melody)
             ((raise-type-error 'arg1 "procedure" instrument))))))

(define (staff-part? x)
  (eq? (get-tag x) 'Staff-Part))

(define (staff-part->instrument staff-part)
  (if (staff-part? staff-part)
      (car (get-item staff-part))
      (raise-type-error 'staff-part->instrument "Staff-Part" staff-part)))

(define (staff-part->melody staff-part)
  (if (staff-part? staff-part)
      (cdr (get-item staff-part))
      (raise-type-error 'staff-part->instrument "Staff-Part" staff-part)))

(define (staff-part->rsound staff-part)
 (if (staff-part? staff-part)
     (melody->rsound
      (staff-part->melody staff-part)
      (instrument->proc (staff-part->instrument staff-part)))
     (raise-type-error 'staff-part->instrument "Staff-Part" staff-part)))


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

;; (define (append-staff staff1 staff2) 1)
;; (define (append-staff* stafflist) 1)


;;;;;;;;;; TESTS ;;;;;;;;;;;;


(define (soft-synth-rsound note)
  (cond ((rest? note)
         (silence (rest->duration note)))
        ((note? note)
         (synth-note "vgame" 76 (note->midi-number note) (note->duration note)))))

(define Sol (make-note 'G 5 (quarter-note-length)))
(define Fa (make-note 'F 5 (quarter-note-length)))
(define Mi (make-note 'Eb 5 (quarter-note-length)))
(define Re (make-note 'D 5 (quarter-note-length)))
(define Do (make-note 'C 5 (quarter-note-length)))
(define Ti (make-note 'B 4 (quarter-note-length)))
(define LowSol (make-note 'G 4 (quarter-note-length)))
(define rest (make-rest (quarter-note-length)))



(define melody1 (make-melody (list Do LowSol Do Re Mi)))
(define melody2 (make-melody (list Mi Re Mi Fa Sol)))

(define soft-synth (make-instrument 'SoftSynth soft-synth-rsound))

(define staff-part1 (make-staff-part soft-synth melody1))
(define staff-part2 (make-staff-part soft-synth melody2))

(define staff1 (make-ensemble-staff (list staff-part1 staff-part2)))

;; Uncomment the following lines to play a short rsound
 (play (e-staff->rsound staff1))



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