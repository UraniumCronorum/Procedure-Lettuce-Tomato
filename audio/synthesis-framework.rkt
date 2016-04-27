;; Copyright (C) David Benoit 2016
;;
;; Author: David Benoit
;;
;; File: synthesis-framework.rkt
;;
;; Description: A Programmable Music Theory 
;;              Interface for Racket's rsound 
;;              library. 
;;
;; 

#lang racket
(require rsound)
(require rsound/piano-tones)
(require rsound/single-cycle)

;; Provide procedures
(provide (all-defined-out))

;;;;;;;  NON MUSICAL UTILITIES ;;;;;;;;;;

;; Attach a tag to an item
(define (attach-tag tag item)
  (cons tag item))

;; Get an object's tag
(define (get-tag object)
  (if (pair? object)
      (car object)
      'unknown))

;; Untag an object
(define (get-item object)
  (if (pair? object)
      (cdr object)
      'error))

;; Take a procedure and call it x times
(define (do-x-times x proc )
  (cond ((< x 2) (proc))
        (else
         (do-x-times proc (- x 1))
         (proc))))


;;;;;;;;; MUSICAL UTILITIES ;;;;;;;;;;;;

;; Take a note letter name and convert it
;; to its relative midi representation 0-11
(define (note-letter-to-base-number letter)
  (cond ((eq? letter 'C) 0)        
        ((eq? letter 'C#) 1)
        ((eq? letter 'C#/Db) 1)
        ((eq? letter 'Db) 1)
        ((eq? letter 'D) 2)
        ((eq? letter 'D#) 3)
        ((eq? letter 'D#/Eb) 3)                
        ((eq? letter 'Eb) 3)
        ((eq? letter 'E) 4)
        ((eq? letter 'F) 5)
        ((eq? letter 'F#) 6)
        ((eq? letter 'F#/Gb) 6)                
        ((eq? letter 'Gb) 6)
        ((eq? letter 'G) 7)
        ((eq? letter 'G#) 8)
        ((eq? letter 'G#/Ab) 8)                
        ((eq? letter 'Ab) 8)
        ((eq? letter 'A) 9)
        ((eq? letter 'A#) 10)
        ((eq? letter 'A#/Bb) 10)        
        ((eq? letter 'Bb) 10)
        ((eq? letter 'B) 11)
        (else -1)))

;; Take a relative midi-representation 0-11
;; and convert it to its letter name
(define (midi-number->letter number)
  (define num (remainder number 12))
  (cond ((eq? num 0) 'C)
        ((eq? num 1)'C#/Db)
        ((eq? num 2) 'D)
        ((eq? num 3)'D#/Eb)
        ((eq? num 4) 'E)
        ((eq? num 5) 'F)
        ((eq? num 6)'F#/Gb)
        ((eq? num 7) 'G)
        ((eq? num 8)'G#/Ab)
        ((eq? num 9) 'A)
        ((eq? num 10)'A#/Bb)
        ((eq? num 11) 'B)
        (else -1)))

;; Get the octave of a midi number
(define (midi-number->octave num)
  (quotient num 12))

;; Take a note and octave and convert it to
;; its exact midi number
(define (letter-and-octave-to-midi letter octave)
  (+ (* octave 12) (note-letter-to-base-number letter)))

;; The default whole note length is the
;; length of an RSound piano tone
(define default-whole-note-length 132300)

;; Adjust the global whole-note length
(define (set-whole-note-length l)
  (set! default-whole-note-length (round l)))

;; Return the length of a whole note in frames
(define (whole-note-length)
  (* default-whole-note-length 1.15))

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

;; Get a note letter and octave and convert
;; it to a pitch in Hz
(define (letter-and-octave-to-freq letter octave)
  (midi-note-num->pitch (letter-and-octave-to-midi letter octave)))

;; Take a beat in frames and subdivide it into equal parts.
;; Return a list of the new subdivisions
(define (subdivide beat subdivision)
  (build-list subdivision (lambda (x) (round (/ beat subdivision)))))


;;;;;;;;;;;; NOTE OBJECT ;;;;;;;;;;;;;;

;; Create a note object from it's letter name (capital symbol),
;; its octave, and its duration in frames
(define (make-note letter octave duration)
  (attach-tag 'Note (list letter octave duration)))

;; Create a note from a midi number and a
;; length in frames
(define (make-note-from-midi-num num length)
  (make-note (midi-number->letter num)
             (midi-number->octave num)
             length))

;; Check if object is a note
(define (note? x)
  (eq? (get-tag x) 'Note))

;; Get letter name of note object
(define (note->letter note)
  (if (note? note)
      (car (get-item note))
      (raise-type-error ' note->letter "Note" note)))

;; Get octave of note object
(define (note->octave note)
  (if (note? note)
      (cadr (get-item note))
      (raise-type-error 'note->octave "Note" note )))

;; Get duration of note object in frames
(define (note->duration note)
  (if (note? note)
      (caddr (get-item note))
      (raise-type-error 'note->duration "Note" note )))

;; Get midi number of note object
(define (note->midi-number note)
  (if (note? note)
      (letter-and-octave-to-midi (note->letter note) (note->octave note))
      (raise-type-error 'note->midi-number "Note" note )))

;; Get frequency of note object in Hz
(define (note->freq note)
  (if (note? note)
      (letter-and-octave-to-freq (note->letter note) (note->octave note))
      (raise-type-error 'note->freq "Note" note)))


;; Take a note object and a procedure which can turn
;; the note into an rsound.  Call the procedure with
;; the note object as an argument
(define (note->rsound note note-to-rsound-proc)
  (cond ((and (note? note) (procedure? note-to-rsound-proc))
           (rs-filter (note-to-rsound-proc note) reverb))
         (else
          (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "Note" note)
             ((raise-type-error 'arg2 "procedure" note-to-rsound-proc))))))


(define (note->interval-up note interval)
  (cond ((eq? interval 'Unison) note)
        ((eq? interval 'AugmentedUnison)
         (make-note-from-midi-num
          (+ (note->midi-number note) 1)
          (note->duration note)))
        ((eq? interval 'Minor2nd)
         (make-note-from-midi-num
          (+ (note->midi-number note) 1)
          (note->duration note)))
        ((eq? interval 'Major2nd)
         (make-note-from-midi-num
          (+ (note->midi-number note) 2)
          (note->duration note)))
        ((eq? interval 'Augmented2nd)
         (make-note-from-midi-num
          (+ (note->midi-number note) 3)
          (note->duration note)))
        ((eq? interval 'Minor3rd)
         (make-note-from-midi-num
          (+ (note->midi-number note) 3)
          (note->duration note)))
        ((eq? interval 'Major3rd)
         (make-note-from-midi-num
          (+ (note->midi-number note) 4)
          (note->duration note)))
        ((eq? interval 'Perfect4th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 5)
          (note->duration note)))
        ((eq? interval 'Augmented4th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 6)
          (note->duration note)))
        ((eq? interval 'Diminished5th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 6)
          (note->duration note)))
        ((eq? interval 'Perfect5th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 7)
          (note->duration note)))
        ((eq? interval 'Augmented5th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 8)
          (note->duration note)))
        ((eq? interval 'Minor6th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 8)
          (note->duration note)))
        ((eq? interval 'Major6th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 9)
          (note->duration note)))
        ((eq? interval 'Minor7th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 10)
          (note->duration note)))
        ((eq? interval 'Major7th)
         (make-note-from-midi-num
          (+ (note->midi-number note) 11)
          (note->duration note)))
        ((eq? interval 'PerfectOctave)
         (make-note-from-midi-num
          (+ (note->midi-number note) 12)
          (note->duration note)))))

(define (note->interval-down note interval)
  (cond ((eq? interval 'Unison) note)
        ((eq? interval 'AugmentedUnison)
         (make-note-from-midi-num
          (- (note->midi-number note) 1)
          (note->duration note)))
        ((eq? interval 'Minor2nd)
         (make-note-from-midi-num
          (- (note->midi-number note) 1)
          (note->duration note)))
        ((eq? interval 'Major2nd)
         (make-note-from-midi-num
          (- (note->midi-number note) 2)
          (note->duration note)))
        ((eq? interval 'Augmented2nd)
         (make-note-from-midi-num
          (- (note->midi-number note) 3)
          (note->duration note)))
        ((eq? interval 'Minor3rd)
         (make-note-from-midi-num
          (- (note->midi-number note) 3)
          (note->duration note)))
        ((eq? interval 'Major3rd)
         (make-note-from-midi-num
          (- (note->midi-number note) 4)
          (note->duration note)))
        ((eq? interval 'Perfect4th)
         (make-note-from-midi-num
          (- (note->midi-number note) 5)
          (note->duration note)))
        ((eq? interval 'Augmented4th)
         (make-note-from-midi-num
          (- (note->midi-number note) 6)
          (note->duration note)))
        ((eq? interval 'Diminished5th)
         (make-note-from-midi-num
          (- (note->midi-number note) 6)
          (note->duration note)))
        ((eq? interval 'PerfectFifth)
         (make-note-from-midi-num
          (- (note->midi-number note) 7)
          (note->duration note)))
        ((eq? interval 'Augmented5th)
         (make-note-from-midi-num
          (- (note->midi-number note) 8)
          (note->duration note)))
        ((eq? interval 'Minor6th)
         (make-note-from-midi-num
          (- (note->midi-number note) 8)
          (note->duration note)))
        ((eq? interval 'Major6th)
         (make-note-from-midi-num
          (- (note->midi-number note) 9)
          (note->duration note)))
        ((eq? interval 'Minor7th)
         (make-note-from-midi-num
          (- (note->midi-number note) 10)
          (note->duration note)))
        ((eq? interval 'Major7th)
         (make-note-from-midi-num
          (- (note->midi-number note) 11)
          (note->duration note)))
        ((eq? interval 'PerfectOctave)
         (make-note-from-midi-num
          (- (note->midi-number note) 12)
          (note->duration note)))))
        

;;;;;;;;;;; REST OBJECT ;;;;;;;;;;;;;;


;; Create a rest (silence) object
(define (make-rest duration)
  (attach-tag 'rest duration))

;; Check if object is a rest object
(define (rest? x)
  (eq? (get-tag x) 'rest))

;; Get duration of the rest object in frames
(define (rest->duration rest)
  (if (rest? rest)
      (get-item rest)
      (raise-type-error 'rest->duration "Rest" rest )))


;;;;;;;;;;;; MEASURE OBJECT ;;;;;;;;;;;;

;; Create a measure object from a list of notes
(define (make-measure notelist)
  (attach-tag
   'Measure (filter
            (lambda (x) (or (note? x) (rest? x) (harmony? x)))
            notelist)))

;; Check if object is a measure
(define (measure? x)
  (eq? (get-tag x) 'Measure))

;; Return the measure's note list
(define (measure->notelist measure)
  (if (measure? measure)
      (get-item measure)
      (raise-type-error 'measure->notelist "Measure" measure)))

;; Take a measure and a procedure that converts notes to rsounds
;; and apply the procedure to all notes in the measure
(define (measure->rsound measure note-to-rsound-proc)
  (cond ((and (measure? measure) (procedure? note-to-rsound-proc))
           (rs-append* (map (lambda (x) (if (harmony? x)
                                           (harmony->rsound x  note-to-rsound-proc)
                                           (note->rsound x note-to-rsound-proc)))
                            (measure->notelist measure))))
         (else
          (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "Measure" measure)
             (raise-type-error 'arg2 "procedure" note-to-rsound-proc)))))

;; Concatenate two measure objects
(define (append-measure m1 m2)
  (cond ((and (measure? m1) (measure? m2))
         (make-measure (append (measure->notelist m1)
                              (measure->notelist m2))))
         (else
           (if (measure? m1)
             (raise-type-error 'arg1 "Measure" m1)
             (raise-type-error 'arg2 "Measure" m1)))))

;; Concatenate a list of measure objects
(define (append-measure* measurelist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (append-measure x y))) '() measurelist))

;; Get the number of notes in a measure object
(define (measure->length measure)
  (if (measure? measure)
      (length (measure->notelist measure))
      (raise-type-error 'measure->length "Measure" measure)))
  

;;;;;;;;;;;;; CHORD OBJECT ;;;;;;;;;;;;;;;;;;;
;; Create a measure object from a list of notes
(define (make-harmony notelist)
  (attach-tag
   'Harmony (filter
            (lambda (x) (or (note? x) (rest? x)))
            notelist)))

;; Check if object is a harmony
(define (harmony? x)
  (eq? (get-tag x) 'Harmony))

;; Return the harmony's note list
(define (harmony->notelist harmony)
  (if (harmony? harmony)
      (get-item harmony)
      (raise-type-error 'harmony->notelist "Harmony" harmony)))

;; Take a harmony and a procedure that converts notes to rsounds
;; and apply the procedure to all notes in the harmony
(define (harmony->rsound harmony note-to-rsound-proc)
  (cond ((and (harmony? harmony) (procedure? note-to-rsound-proc))
           (rs-overlay* (map note-to-rsound-proc (harmony->notelist harmony))))
         (else
          (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "Harmony" harmony)
             (raise-type-error 'arg2 "procedure" note-to-rsound-proc)))))

;; Merge two harmony objects
(define (merge-harmony m1 m2) 
  (cond ((and (harmony? m1) (harmony? m2))
         (make-harmony (append (harmony->notelist m1) 
                              (harmony->notelist m2))))
         (else
           (if (harmony? m1) 
             (raise-type-error 'arg1 "Harmony" m1) 
             (raise-type-error 'arg2 "Harmony" m1)))))

;; Merge a list of harmony objects
(define (merge-harmony* harmonylist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (merge-harmony x y))) '() harmonylist))

;; Get number of notes in a harmony
(define (harmony->size harmony)
  (if (harmony? harmony)
      (length (harmony->notelist harmony))
      (raise-type-error 'harmony->size "Harmony" harmony)))

;;;;;;;;;;;;; INSTRUMENT OBJECT ;;;;;;;;;;;;;;

;; Create an instrument object which holds a name (symbol) and a
;; procedure that converts notes to rsounds
(define (make-instrument name note-to-rsound-proc)
  (cond ((and (procedure? note-to-rsound-proc) (symbol? name))
         (attach-tag 'Instrument (cons name note-to-rsound-proc)))
        (else
         (if (procedure? note-to-rsound-proc)
             (raise-type-error 'arg1 "symbol" name)
             ((raise-type-error 'arg2 "procedure" note-to-rsound-proc))))))

;; Check if object is an instrument object
(define (instrument? x)
  (eq? (get-tag x) 'Instrument))

;; Get instrument name (symbol)
(define (instrument->name instrument)
  (if (instrument? instrument)
      (car (get-item instrument))
      (raise-type-error 'instrument->name "Instrument" instrument)))

;; Take an instrument's procedure and wrap it to ensure it
;; can handle rest objects
(define (instrument-proc-wrapper instrument-proc)
  (lambda (note)
    (if (rest? note)
        (silence (rest->duration note))
        (instrument-proc note))))

;; Return the instrument's internal conversion procedure
(define (instrument->proc instrument)
  (if (instrument? instrument)
      (instrument-proc-wrapper
       (cdr (get-item instrument)))
      (raise-type-error 'instrument->proc "Instrument" instrument)))


;;;;;;;;;;;;;; STAFF PART OBJECT ;;;;;;;;;;;;;;;;;

;; Create a staff-part object from an instrument and a
;; list fo measures
(define (make-staff-part instrument measurelist)
  (cond ((instrument? instrument)
         (attach-tag 'StaffPart (cons instrument (filter measure? measurelist))))
        (else
         (raise-type-error 'make-staff-part "procedure" instrument))))

;; Check if object is a staff-part object
(define (staff-part? x)
  (eq? (get-tag x) 'StaffPart))

;; Get the staff-part's instrument
(define (staff-part->instrument staff-part)
  (if (staff-part? staff-part)
      (car (get-item staff-part))
      (raise-type-error 'staff-part->instrument "StaffPart" staff-part)))

;;  Get the staff-part's measures
(define (staff-part->measurelist staff-part)
  (if (staff-part? staff-part)
      (cdr (get-item staff-part))
      (raise-type-error 'staff-part->instrument "StaffPart" staff-part)))

;; Convert a staff-part into an rsound
(define (staff-part->rsound staff-part)
 (if (staff-part? staff-part)
     (measure->rsound
      (append-measure*
       (staff-part->measurelist staff-part))
      (instrument->proc (staff-part->instrument staff-part)))
     (raise-type-error 'staff-part->instrument "StaffPart" staff-part)))

;; Concatenate two staff-parts
(define (append-staff-part part1 part2)
    (cond ((and (staff-part? part1) (staff-part? part2))
         (make-staff-part (staff-part->instrument part1)
                          (append (staff-part->measurelist part1)
                                  (staff-part->measurelist part2))))
         (else
           (if (staff-part? part1)
             (raise-type-error 'arg1 "StaffPart" part2)
             (raise-type-error 'arg2 "StaffPart" part1)))))

;; Concatenate a list of staff-parts
(define (append-staff-part* partlist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (append-staff-part x y))) '() partlist))

;;;;;;;;;;;;;; ENSEMBLE STAFF OBJECT ;;;;;;;;;;;;;;;;

;; Create an ensemble part object
(define (make-ensemble-staff partlist)
  (attach-tag
   'EnsembleStaff (filter (lambda (x) (staff-part? x)) partlist)))

;; Check if object is an ensemble part object
(define (e-staff? x)
  (eq? (get-tag x) 'EnsembleStaff))

;; Get an ensemble-staff's list of staff parts
(define (e-staff->partlist staff)
  (if (e-staff? staff)
      (get-item staff)
      (raise-type-error 'e-staff->instrument "EnsembleStaff" staff)))

;; Convert an ensemble-staff object into an rsound
(define (e-staff->rsound staff)
  (rs-overlay* (map staff-part->rsound (e-staff->partlist staff))))

;; Concatenate two ensemble staff objects
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

;; Concatenate a list of ensemble-staff objects
(define (append-ensemble-staff* e-stafflist)
  (foldl (lambda (x y)
           (if (eq? y '())
               x
                (append-ensemble-staff x y))) '() e-stafflist))

;;;;;;;;;; SYNTHESIZERS ;;;;;;;;;;;;

;; Create a procedure that converts a note to
;; an rsound with the waveform of rsound/single-cycle's
;; vgame synth #82
(define (soft-synth-rsound note)
  (cond ((rest? note)
         (silence (rest->duration note)))
        ((note? note)
         (synth-note "vgame" 82 (note->midi-number note) (note->duration note)))))

(define soft-synth (make-instrument 'SoftSynth soft-synth-rsound))

(define (new-instrument-proc note number)
  (cond ((rest? note)
         (silence (rest->duration note)))
        ((note? note)
         (synth-note "vgame" (random 1 110) (note->midi-number note) (note->duration note)))))

(define
 (random-synth)
  (let ((random-sound (random 114 115)))
    (make-instrument
     'RandomInstrument
     (lambda (note) (new-instrument-proc note random-sound)))))

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
