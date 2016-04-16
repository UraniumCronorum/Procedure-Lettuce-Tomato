#lang racket
(require rsound)
(require rsound/single-cycle)
(require "rsound-music-theory.rkt")



(define (gen-beat-structure length)
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
         
(define (gen-random-notes length min max)
  (let ([beat-structure (gen-beat-structure length)])
        (map (lambda (x) (make-note-from-midi-num
                          (random min max) x)) beat-structure)))

(define (gen-random-measure min max)
  (make-measure (gen-random-notes (whole-note-length) min max)))

(define part1
  (make-staff-part soft-synth
   (build-list 10 (lambda (x) (gen-random-measure 60 72)))))

(define part2
  (make-staff-part soft-synth
   (build-list 10 (lambda (x) (gen-random-measure 36 48)))))

(define staff1
  (make-ensemble-staff
   (list part1 part2)))

(play (e-staff->rsound staff1))