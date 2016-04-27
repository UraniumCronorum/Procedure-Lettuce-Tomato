;; Copyright (C) David Benoit 2016
;;
;; Author: David Benoit
;;
;; File: tonal-harmony.rky
;;
;; Description: Create a basic chord and progression
;;              interface for rsound
;;
;; Note: Currently only supports root position
;;       clised voicings

#lang racket

(require rsound)
(require "synthesis-framework.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;; Basic Chord ;;;;;;;;;;;;

;; Make a harmony object with the notes of a specified chord
(define (make-basic-chord root quality duration)
  (define new-root (make-note (note->letter root) (note->octave root) duration))
  (define (major-triad)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Major3rd)
      (note->interval-up new-root 'Perfect5th))))
  (define (minor-triad)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Minor3rd)
      (note->interval-up new-root 'Perfect5th))))
  (define (diminished-triad)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Minor3rd)
      (note->interval-up new-root 'Diminished5th))))
  (define (augmented-triad)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Major3rd)
      (note->interval-up new-root 'Augmented5th))))
  (define (major-seventh)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Major3rd)
      (note->interval-up new-root 'Perfect5th)
      (note->interval-up new-root 'Major7th))))
  (define (minor-seventh)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Minor3rd)
      (note->interval-up new-root 'Perfect5th)
      (note->interval-up new-root 'Minor7th))))
  (define (dominant-seventh)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Major3rd)
      (note->interval-up new-root 'Perfect5th)
      (note->interval-up new-root 'Minor7th))))
  (define (half-diminished)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Minor3rd)
      (note->interval-up new-root 'Diminished5th)
      (note->interval-up new-root 'Minor7th))))
  (define (diminished-seventh)
    (make-harmony
     (list
      new-root
      (note->interval-up new-root 'Minor3rd)
      (note->interval-up new-root 'Diminished5th)
      ;; No support for diminished seventh
      ;; interval yet, so use major 6th
      (note->interval-up new-root 'Major6th))))
  (define (dispatch token)
    (cond ((eq? token 'Maj) (major-triad))
          ((eq? token 'Min) (minor-triad))
          ((eq? token 'Dim) (diminished-triad))
          ((eq? token 'Aug) (augmented-triad))
          ((eq? token 'Maj7) (major-seventh))
          ((eq? token 'Min7) (minor-seventh))
          ((eq? token 'Dom7) (dominant-seventh))
          ((eq? token 'HalfDim) (half-diminished))
          ((eq? token 'Dim7) (diminished-seventh))
          (else
           ((make-harmony new-root)))))
  (dispatch quality))

;;;;;;;;;;;;;;;; Functional Harmony ;;;;;;;;;;;;;;

;; Take a key (note object), roman numeral (symbol),
;; and duration in frames and produce a harmony object
(define (make-functional-harmony key roman-numeral duration)
  (define (I)
    (make-basic-chord key 'Maj duration))
  (define (i)
    (make-basic-chord key 'Min duration))
  (define (I7)
    (make-basic-chord key 'Maj7 duration))
  (define (i7)
    (make-basic-chord key 'Min7 duration))
  (define V/IV I)
  (define (V7/IV)
    (make-basic-chord key 'Dom7 duration))  
  (define (ii)
    (make-basic-chord
     (note->interval-up key 'Major2nd)
     'Min
     duration))
  (define (ii-dim)
    (make-basic-chord
     (note->interval-up key 'Major2nd)
     'Dim
     duration))
  (define (V/V)
    (make-basic-chord
     (note->interval-up key 'Major2nd)
     'Maj
     duration))
  (define (ii7)
    (make-basic-chord
     (note->interval-up key 'Major2nd)
     'Min7
     duration))
  (define (ii-dim7)
    (make-basic-chord
     (note->interval-up key 'Major2nd)
     'HalfDim
     duration))
  (define (bII)
    (make-basic-chord
     (note->interval-up key 'Minor2nd)
     'Maj
     duration))
  (define (V7/V)
    (make-basic-chord
     (note->interval-up key 'Major2nd)
     'Dom7
     duration))
  (define (iii)
    (make-basic-chord
     (note->interval-up key 'Major3rd)
     'Min
     duration))
  (define (bIII)
    (make-basic-chord
     (note->interval-up key 'Minor3rd)
     'Maj
     duration))
  (define (V7/VI)
    (make-basic-chord
     (note->interval-up key 'Major3rd)
     'Dom7
     duration))
  (define (IV)
    (make-basic-chord
     (note->interval-up key 'Perfect4th)
     'Maj
     duration))
  (define (iv)
    (make-basic-chord
     (note->interval-up key 'Perfect4th)
     'Min
     duration))
  (define (V)
    (make-basic-chord
     (note->interval-up key 'Perfect5th)
     'Maj
     duration))
  (define (V7)
    (make-basic-chord
     (note->interval-up key 'Perfect5th)
     'Dom7
     duration))
  (define (vi)
    (make-basic-chord
     (note->interval-up key 'Major6th)
     'Min
     duration))
  (define (bVI)
    (make-basic-chord
     (note->interval-up key 'Minor6th)
     'Maj
     duration))
  (define (vii-dim)
    (make-basic-chord
     (note->interval-up key 'Major7th)
     'Dim
     duration))
  (define (bVII)
    (make-basic-chord
     (note->interval-up key 'Minor7th)
     'Maj
     duration))
  (define (dispatch token)
    (cond ((eq? token 'I) (I))
          ((eq? token 'i) (i))
          ((eq? token 'I7) (I7))
          ((eq? token 'i7) (i7))
          ((eq? token 'V/IV) (V/IV))
          ((eq? token 'V7/IV) (V7/IV))
          ((eq? token 'ii) (ii))
          ((eq? token 'ii-dim) (ii-dim))
          ((eq? token 'ii7) (ii7))
          ((eq? token 'ii-dim7) (ii-dim7))
          ((eq? token 'bII) (bII))
          ((eq? token 'V7/V) (V7/V))
          ((eq? token 'iii) (iii))
          ((eq? token 'bIII) (bIII))
          ((eq? token 'bIII) (bIII))
          ((eq? token 'V7/VI) (V7/VI))
          ((eq? token 'IV) (IV))
          ((eq? token 'iv) (iv))
          ((eq? token 'V) (V))
          ((eq? token 'V7) (V7))
          ((eq? token 'vi) (vi))
          ((eq? token 'bVI) (bVI))
          ((eq? token 'vii-dim) (vii-dim))
          ((eq? token 'bVII) (bVII))
          (else
           (make-harmony key))))
  (dispatch roman-numeral))

;; Alias make-measure as harmonic-progression 
(define make-harmonic-progression make-measure)
(define append-progression* append-measure*)

;; Alias notelist as chordlist
(define progression->chordlist measure->notelist)


;;;;;;;;;; Common Minor Progressions ;;;;;;;;;

(define (i-V7-bVI-V key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'V7 chord-duration)
    (make-functional-harmony key 'bVI chord-duration)
    (make-functional-harmony key 'V chord-duration))))

(define (i-iv-V7 key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'iv chord-duration)
    (make-functional-harmony key 'V7 chord-duration))))

(define (i-bVI-iidim-V7-i-iv-V7 key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'bVI chord-duration)
    (make-functional-harmony key 'ii-dim chord-duration)
    (make-functional-harmony key 'V chord-duration)
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'iv chord-duration)
    (make-functional-harmony key 'V7 chord-duration))))

(define (i-bVI-iidim-V7 key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'bVI chord-duration)
    (make-functional-harmony key 'ii-dim chord-duration)
    (make-functional-harmony key 'V chord-duration))))

(define (i-iv-V7-i-bVI-iidim-V7 key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'iv chord-duration)
    (make-functional-harmony key 'V7 chord-duration)
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'bVI chord-duration)
    (make-functional-harmony key 'ii-dim chord-duration)
    (make-functional-harmony key 'V chord-duration))))

(define (i-iv-V7-i-bVI-bII-V7 key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'iv chord-duration)
    (make-functional-harmony key 'V7 chord-duration)
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'bVI chord-duration)
    (make-functional-harmony key 'bII chord-duration)
    (make-functional-harmony key 'V chord-duration))))


(define (i-iv-bVII-III-bVI-iidim-V key chord-duration)
  (make-harmonic-progression
   (list
    (make-functional-harmony key 'i chord-duration)
    (make-functional-harmony key 'iv chord-duration)
    (make-functional-harmony key 'bVII chord-duration)
    (make-functional-harmony key 'bIII chord-duration)
    (make-functional-harmony key 'bVI chord-duration)
    (make-functional-harmony key 'ii-dim chord-duration)
    (make-functional-harmony key 'V chord-duration))))


;; Put the progressions in a list for easy access
(define tonal-proglist
  (list i-iv-V7
        i-V7-bVI-V
        i-bVI-iidim-V7
        i-iv-V7-i-bVI-bII-V7
        i-iv-V7-i-bVI-iidim-V7
        i-bVI-iidim-V7-i-iv-V7
        i-iv-bVII-III-bVI-iidim-V
        ))
        


