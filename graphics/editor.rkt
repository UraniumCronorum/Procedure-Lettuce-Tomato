#lang racket/gui

(require "./sprites.rkt"
         racket/serialize
         racket/dict
         racket/format)

(serializable-struct path (start-point point-data style-data))


;                                                                                      
;              ;                                                                       
;   ;;;;;      ;     ;     ;                           ;;;  ;;;                        
;   ;          ;           ;                          ;   ;   ;                        
;   ;       ;;;;   ;;;   ;;;;;   ;;;    ;;;;         ;        ;    ;;;;    ;;;    ;;;  
;   ;      ;; ;;     ;     ;    ;; ;;   ;;  ;        ;        ;        ;  ;   ;  ;   ; 
;   ;;;;;  ;   ;     ;     ;    ;   ;   ;            ;        ;        ;  ;      ;     
;   ;      ;   ;     ;     ;    ;   ;   ;            ;        ;     ;;;;   ;;;    ;;;  
;   ;      ;   ;     ;     ;    ;   ;   ;            ;        ;    ;   ;      ;      ; 
;   ;      ;; ;;     ;     ;    ;; ;;   ;             ;   ;   ;    ;   ;  ;   ;  ;   ; 
;   ;;;;;   ;;;;   ;;;;;   ;;;   ;;;    ;              ;;;     ;;   ;;;;   ;;;    ;;;  
;                                                                                      
;                                                                                      
;                                                                                      

(define-serializable-class editor%
  object%

  ;; initializer
  (init [name "Untitled"] [width 0] [height 0])
  (define _name name)
  (define _width width)
  (define _height height)
  (define all-animations #hash())
  (define current-animation-name #f)
  (define current-frame-index #f)
  (define current-path-name #f)
  (define current-point-index #f)
  (super-new)

  ;; accessors
  (define/public (get-name) _name)
  (define/public (get-width) _width)
  (define/public (get-height) _height)
  (define/public (get-frame-index) current-frame-index)
  
  ;;; Animation
  
  ;; Animation Selection
  (define/public (get-animation-names) (dict-keys all-animations))
  (define/public (get-all-animations) (dict-values all-animations))
  (define/public (select-animation name)
    (set! current-animation-name name)
    (if (or (false? (get-current-animation))
            (= (vector-length (get-current-animation)) 0))
        (set! current-frame-index #f)
        (set! current-frame-index 0))
    (set! current-path-name #f)
    (set! current-point-index #f))
  (define/public (get-current-animation)
    (if current-animation-name
        (dict-ref all-animations current-animation-name)
        #f))
  
  ;; Animation Updating
  (define/public (update-current-animation new)
    (if current-animation-name
        (set! all-animations (dict-set all-animations current-animation-name new))
        (error "No animation to update")))
  
  ;; Adding and Removing Animations
  (define/public (add-animation name)
    (set! all-animations (dict-set all-animations name #())))
  
  (define/public (delete-animation)
    (if current-animation-name
        (set! all-animations (dict-remove all-animations current-animation-name))
        (void))
    (set! current-animation-name #f)
    (set! current-frame-index #f)
    (set! current-path-name #f)
    (set! current-point-index #f))
  
  ;;; Frame
  
  ;; Frame Selection
  (define/public (get-current-frame)
    (if (and (get-current-animation) (> (vector-length (get-current-animation)) 0) current-frame-index)
        (vector-ref (get-current-animation) current-frame-index)
        #f))
  
  (define/public (next-frame)
    (if (and current-frame-index (< (+ current-frame-index 1) (vector-length (get-current-animation))))
        (set! current-frame-index (+ current-frame-index 1))
        (void))
    (set! current-path-name #f)
    (set! current-point-index #f))
  
  (define/public (prev-frame)
    (if (and current-frame-index (> current-frame-index 0))
        (set! current-frame-index (- current-frame-index 1))
        (void))
    (set! current-path-name #f)
    (set! current-point-index #f))
  
  ;; Frame Updating
  (define/public (update-current-frame new)
    (if current-frame-index
        (vector-set! (get-current-animation) current-frame-index new)
        ;; else
        (error "No frame to update")))
  
  ;; Add and Remove Frames
  (define/public (add-frame)
    (if (get-current-animation)
        (let ([new-frame-index (if (get-current-animation) (vector-length (get-current-animation)) 0)]
              [new-frame (if (get-current-frame) (get-current-frame) #hash())])
          (update-current-animation (vector-append (get-current-animation) (vector new-frame)))
          (set! current-frame-index new-frame-index))
        (void)))
  
  (define/public (insert-frame)
    ;; not implemented yet
    (if (get-current-animation)
        (let ([new-frame-index (if current-frame-index current-frame-index 0)]
              [new-frame (if (get-current-frame) (get-current-frame) #hash())]
              [before (vector-take (get-current-animation) current-frame-index)]
              [after (vector-drop (get-current-animation) current-frame-index)])
          (vector-append before (vector new-frame) after)
          (set! current-frame-index new-frame-index))
        (void)))
  
  (define/public (delete-frame)
    (if (get-current-animation)
        (begin
          (if current-frame-index
              (let ([new-frame-index (if (> current-frame-index 0) (- current-frame-index 1) 0)]
                    [before (vector-take (get-current-animation) current-frame-index)]
                    [after (vector-drop (get-current-animation) (+ current-frame-index 1))])
                (update-current-animation (vector-append before after))
                (set! current-frame-index new-frame-index))
              ;; else
              (void))
          (if (= (vector-length (get-current-animation)) 0)
              (set! current-frame-index #f)
              (void))
          (set! current-path-name #f)
          (set! current-point-index #f))
        (void)))
    
  ;;; Path
  
  ;; Selection
  (define/public (get-path-names)
    (if (get-current-frame)
        (dict-keys (get-current-frame))
        '()))
  
  (define/public (select-path name)
    (if (get-current-frame)
        (set! current-path-name name)
        (error "Can't select path; no current frame"))
    (set! current-point-index #f))
  
  (define/public (get-current-path)
    (if (and (get-current-frame) current-path-name)
        (dict-ref (get-current-frame) current-path-name)
        #f))
  
  ;; Updating
  (define/public (update-current-path new-path)
    (update-current-frame (dict-set (get-current-frame) current-path-name new-path)))
  
  ;; Adding and Removing
  (define/public (add-path name)
    (if (get-current-frame)
        (update-current-frame (dict-set (get-current-frame)
                                        name
                                        ;; default path
                                        (path '(0 0) #() '("white" "black"))))
        (void)))
  
  (define/public (delete-path)
    (if (and current-path-name (get-current-frame))
        (update-current-frame (dict-remove (get-current-frame) current-path-name))
        (void))
    (set! current-path-name #f))
  
  
  ;;; Point
  
  ;; Selection and Get-info
  (define/public (select-point index)
    (if (get-current-path)
        (set! current-point-index index)
        (error "No path selected")))
  
  (define/public (get-current-point)
    (if (and (get-current-path) current-point-index)
        (vector-ref (path-point-data (get-current-path)) current-point-index)
        (void)))
  
  (define/public (get-all-points)
    (if (get-current-path) (path-point-data (get-current-path)) '()))
  (define/public (get-start-point)
    (if (get-current-path) (path-start-point (get-current-path)) #f))
  (define/public (get-style)
    (if (get-current-path) (path-style-data (get-current-path)) #f))
  
  (define/public (get-all-x-y)
    (if (get-current-path)
        (all-points (let ([start-data (path-start-point (get-current-path))])
                      (make-object point% (first start-data) (last start-data)))
                    (vector->list (path-point-data (get-current-path))))
        '()))
  
  ;; Add points
  (define/public (add-point)
    (if (get-current-path)
        (begin
          (if current-point-index (void) (set! current-point-index 0))
          (update-current-path (let* ([current (get-current-path)]
                                      [new-point-data (vector-append (path-point-data current) (vector '(0 r 0)))])
                                 (path (path-start-point current) new-point-data (path-style-data current)))))
        (void))
    (if (and (get-current-path) (eq? #() (path-point-data (get-current-path))))
        (set! current-point-index #f)
        (void)))
  
  (define/public (insert-point)
    (if (get-current-path)
        (begin
          (if current-point-index (void) (set! current-point-index 0))
          (update-current-path (let* ([current (get-current-path)]
                                      [before (vector-take (path-point-data current) current-point-index)]
                                      [after (vector-drop (path-point-data current) current-point-index)]
                                      [new-point-data (vector-append before (vector '(0 r 0)) after)])
                                 (path (path-start-point current) new-point-data (path-style-data current)))))
        (void))
    (if (and (get-current-path) (eq? #() (path-point-data (get-current-path))))
        (set! current-point-index #f)
        (void)))

  (define/public (delete-point)
    (if (and (get-current-path) (> (vector-length (path-point-data (get-current-path))) 0))
        (begin
          (if current-point-index (void) (set! current-point-index 0))
          (update-current-path (let* ([current (get-current-path)]
                                      [before (vector-take (path-point-data current) current-point-index)]
                                      [after (vector-drop (path-point-data current) (+ current-point-index 1))])
                                 (path (path-start-point current) (vector-append before after) (path-style-data current)))))
        (void))
    (if (and (get-current-path) (eq? #() (path-point-data (get-current-path))))
        (set! current-point-index #f)
        (void)))
  
  ;;; Actual editing functions
  (define/public (change-color #:brush brush #:pen pen)
    (update-current-path (path (path-start-point (get-current-path))
                               (path-point-data (get-current-path))
                               (list brush pen))))
  
  (define/public (change-start x y)
    (if x (void) (set! x (first (get-start-point))))
    (if y (void) (set! y (last (get-start-point))))
    (update-current-path (path (list x y)
                               (path-point-data (get-current-path))
                               (path-style-data (get-current-path)))))
  
  (define/public (change-current #:s slope #:d direction #:m magnitude)
    (if slope (void) (set! slope (first (get-current-point))))
    (if (eq? direction '||) (set! direction (second (get-current-point))) (void))
    (if magnitude (void) (set! magnitude (third (get-current-point))))
    (update-current-path (path (path-start-point (get-current-path))
                               (vector-append (vector-take (path-point-data (get-current-path)) current-point-index)
                                              (vector (list slope direction magnitude))
                                              (vector-drop (path-point-data (get-current-path)) (+ current-point-index 1)))
                               (path-style-data (get-current-path)))))

  ;;; Sprite Frame
  (define/public (make-sprite-frame)
    (let ([out (new sprite-frame% [_width _width] [_height _height] [_name _name])])
      (if (get-current-frame)
          (for ([path-name (dict-keys (get-current-frame))])
            (let ([path (dict-ref (get-current-frame) path-name)])
              (send out add-path path-name
                    (multiple-line-path (all-points (let ([start-data (path-start-point path)])
                                                      (make-object point% (first start-data) (last start-data)))
                                                    (vector->list (path-point-data path))))
                    #:brush (make-brush #:color (first (path-style-data path))) #:pen (make-pen #:color (second (path-style-data path))))))
          (void))
      out))

  ;; PNG output
  (define/public (sprite-frame frame name)
    (let ([out (new sprite-frame% [_width _width] [_height _height] [_name name])])
      (for ([path-name (dict-keys frame)])
        (let ([path (dict-ref frame path-name)])
          (send out add-path path-name
                (multiple-line-path (all-points (let ([start-data (path-start-point path)])
                                                  (make-object point% (first start-data) (last start-data)))
                                                (vector->list (path-point-data path))))
                #:brush (make-brush #:color (first (path-style-data path))) #:pen (make-pen #:color (second (path-style-data path))))))
      out))

  (define/public (make-pngs)
    (for ([animation-name (dict-keys all-animations)])
      (for ([frame (dict-ref all-animations animation-name)]
            [i (in-range (vector-length (dict-ref all-animations animation-name)))])
        (let ([s-frame (sprite-frame frame (string-append _name "-" animation-name "-" (number->string i)))])
          (send s-frame write-file)))))

  ;; Save and Load
  (define/public (save)
    (define destination (open-output-file (string-append _name ".serialized")
                                        #:exists 'truncate))
    (define out (list _name _width _height all-animations))
    (write (serialize out) destination))

  (define/public (load filename)
    (define source (open-input-file (string-append filename ".serialized")))
    (define in (deserialize (read source)))
    (set! _name (first in))
    (set! _width (second in))
    (set! _height (third in))
    (set! all-animations (fourth in))))

(define (save editor)
  (send editor save))

(define (load filename)
  (let ([out (new editor%)])
    (send out load filename)
    out))


;                                                                                                           
;              ;                                                                                            
;   ;;;;;      ;     ;     ;                         ;;;;;                         ;       ;                
;   ;          ;           ;                         ;                             ;                        
;   ;       ;;;;   ;;;   ;;;;;   ;;;    ;;;;         ;      ;   ;  ; ;;    ;;;   ;;;;;   ;;;    ;;;   ; ;;  
;   ;      ;; ;;     ;     ;    ;; ;;   ;;  ;        ;      ;   ;  ;;  ;  ;;  ;    ;       ;   ;; ;;  ;;  ; 
;   ;;;;;  ;   ;     ;     ;    ;   ;   ;            ;;;;;  ;   ;  ;   ;  ;        ;       ;   ;   ;  ;   ; 
;   ;      ;   ;     ;     ;    ;   ;   ;            ;      ;   ;  ;   ;  ;        ;       ;   ;   ;  ;   ; 
;   ;      ;   ;     ;     ;    ;   ;   ;            ;      ;   ;  ;   ;  ;        ;       ;   ;   ;  ;   ; 
;   ;      ;; ;;     ;     ;    ;; ;;   ;            ;      ;   ;  ;   ;  ;;       ;       ;   ;; ;;  ;   ; 
;   ;;;;;   ;;;;   ;;;;;   ;;;   ;;;    ;            ;       ;;;;  ;   ;   ;;;;    ;;;   ;;;;;  ;;;   ;   ; 
;                                                                                                           
;                                                                                                           
;                                                                                                           

(define (gui-editor editor)

  ;;; Path and Point Window ;;;
  
  (define path-window (new frame% [label "Path Editor"]))
  (define path-window-panel (new horizontal-panel% [parent path-window]))
  (define x-y-panel (new vertical-panel% [parent path-window-panel]))
  (define points-panel (new vertical-panel% [parent path-window-panel]))
  (define path-panel (new vertical-panel% [parent path-window-panel]))
  
  ;;; X-Y point data
  
  (define x-y-list
    (new list-box% [parent x-y-panel] [label #f] [choices '()]))
  (define start-point-panel (new horizontal-panel% [parent x-y-panel] [alignment '(center center)] [stretchable-height #f]))
  (define start-x (new text-field% [parent start-point-panel] [label "start x"] [style '(single vertical-label)]))
  (define start-y (new text-field% [parent start-point-panel] [label "start y"] [style '(single vertical-label)]))
  ;; function
  (define (update-x-y-list)
    (send x-y-list clear)
    (for ([point (send editor get-all-x-y)])
      (send x-y-list append
            (~a (cons (send point get-x) (send point get-y))))))
  ;; button
  (new button% [parent x-y-panel] [label "update start point"]
       [callback (lambda (button event)
                   (send editor change-start
                         (string->number (send start-x get-value))
                         (string->number (send start-y get-value)))
                   (update-x-y-list))])
  
  ;;; Points
  
  (define points-list
    (new list-box% [parent points-panel] [label "Points"] [choices '()]
         [callback (lambda (list-box event)
                     (send editor select-point (send list-box get-selection)))]))
  (define point-button-panel (new horizontal-panel% [parent points-panel] [stretchable-height #f] [alignment '(center center)]))
  ;; functions
  (define (update-points-list)
    (send points-list clear)
    (for ([point (send editor get-all-points)])
      (send points-list append
            (~a point)))
    (update-x-y-list))
  ;; buttons
  (new button% [parent point-button-panel] [label "Add Point"]
       [callback (lambda (button event)
                   (send editor add-point)
                   (update-points-list))])
  (new button% [parent point-button-panel] [label "Insert Point"]
       [callback (lambda (button event)
                   (send editor insert-point)
                   (update-points-list))])
  (new button% [parent point-button-panel] [label "Remove Point"]
       [callback (lambda (button event)
                   (send editor delete-point)
                   (update-points-list))])
  ;; Point editor
  (define point-update-panel (new horizontal-panel% [parent points-panel] [stretchable-height #f]))
  (define slope (new text-field% [parent point-update-panel] [label "slope"] [style '(single vertical-label)]))
  (define direction (new text-field% [parent point-update-panel] [label "direction"] [style '(single vertical-label)]))
  (define magnitude (new text-field% [parent point-update-panel] [label "magnitude"] [style '(single vertical-label)]))
  (new button% [parent points-panel] [label "Update Point"]
       [callback (lambda (button event)
                   (send editor change-current
                         #:s (string->number (send slope get-value))
                         #:d (string->symbol (send direction get-value))
                         #:m (string->number (send magnitude get-value)))
                   (update-points-list))])
  
  ;;; Path
  
  (define path-box
    (new list-box% [parent path-panel] [label "Paths"] [choices '()]
         [callback (lambda (list-box event)
                     (send editor select-path (send list-box get-string-selection))
                     (update-points-list))]))
  (define new-path
    (new horizontal-panel% [parent path-panel] [stretchable-height #f]))
  (define new-path-name
    (new text-field% [parent new-path] [label "Name:"]))
  ;; functions
  (define (update-path-box)
    (send path-box clear)
    (for ([key (send editor get-path-names)])
      (send path-box append key))
    (update-points-list))
  ;; buttons
  (new button% [parent new-path] [label "Add Path"]
       [callback (lambda (button event)
                   (send editor add-path (send new-path-name get-value))
                   (update-path-box))])
  (new button% [parent path-panel] [label "Delete Path"]
       [callback (lambda (button event)
                   (send editor delete-path)
                   (update-path-box))])
  ;; Path editor
  (define brush-box (new text-field% [parent path-panel] [label "Brush"]))
  (define pen-box (new text-field% [parent path-panel] [label "Pen"]))
  (new button% [parent path-panel] [label "Update Path"]
       [callback (lambda (button event)
                   (send editor change-color #:brush (send brush-box get-value) #:pen (send pen-box get-value)))])

  ;;; Frame, Canvas, and Animation ;;;
  
  (define editor-frame (new frame% [label (send editor get-name)]))
  
  (define animation-frame-panel (new horizontal-panel% [parent editor-frame]))
  (define canvas-panel (new vertical-panel% [parent animation-frame-panel]))
  (define animation-panel (new vertical-panel% [parent animation-frame-panel]))
  
  ;;; Canvas & Frame Selector
  
  (define frame-indicator (new message% [parent canvas-panel] [label "No frames"]))
  (define canvas
    (new canvas% [min-width (send editor get-width)] [min-height (send editor get-height)] [parent canvas-panel]
         [paint-callback (lambda (canvas dc)
                           (let ([sprite-frame (send editor make-sprite-frame)])
                             (send sprite-frame output-to-dc dc)))]))
  ;; functions for buttons
  (define (update-frame-indicator)
    (let ([frame-index (send editor get-frame-index)])
      (if frame-index
          (send frame-indicator set-label (string-append "Frame: "(number->string frame-index)))
          (send frame-indicator set-label "No frames"))
      (update-path-box)))
  ;; buttons
  (define frame-selectors (new horizontal-panel% [parent canvas-panel] [alignment '(center center)] [stretchable-height #f]))
  (new button% [parent frame-selectors] [label "Previous"]
       [callback (lambda (button event)
                   (send editor prev-frame)
                   (update-frame-indicator)
                   (send canvas refresh-now))])
  (new button% [parent frame-selectors] [label "Next"]
       [callback (lambda (button event)
                   (send editor next-frame)
                   (update-frame-indicator)
                   (send canvas refresh-now))])
  (new button% [parent canvas-panel] [label "Add Frame"]
       [callback (lambda (button event)
                   (send editor add-frame)
                   (update-frame-indicator))])
  (new button% [parent canvas-panel] [label "Insert Frame"]
       [callback (lambda (button event)
                   (send editor insert-frame)
                   (update-frame-indicator))])
  (new button% [parent canvas-panel] [label "Delete Frame"]
       [callback (lambda (button event)
                   (send editor delete-frame)
                   (update-frame-indicator))])
  
  ;;; Animation
  
  (define animation-box
    (new list-box% [parent animation-panel] [label "Animations"] [choices (send editor get-animation-names)]
         [callback (lambda (list-box event)
                     (send editor select-animation (send list-box get-string-selection))
                     (update-frame-indicator))]))
  (define new-animation
    (new horizontal-panel% [parent animation-panel] [stretchable-height #f]))
  (define new-animation-name
    (new text-field% [parent new-animation] [label "Name:"]))
  ;; functions
  (define (update-animation-box)
    (send animation-box clear)
    (for ([key (send editor get-animation-names)])
      (send animation-box append key)
      (update-frame-indicator)))
  ;; buttons
  (new button% [parent new-animation] [label "Create new animation"]
       [callback (lambda (button event)
                   (send editor add-animation (send new-animation-name get-value))
                   (update-animation-box))])
  (new button% [parent animation-panel] [label "Delete animation"]
       [callback (lambda (button event)
                   (send editor delete-animation)
                   (update-animation-box))])

  
  ;; Display Windows
  (send path-window show #t)
  (send editor-frame show #t))


;                              
;                              
;  ;;  ;;            ;         
;  ;;  ;;                      
;  ;;  ;;  ;;;;    ;;;   ; ;;  
;  ; ;; ;      ;     ;   ;;  ; 
;  ; ;; ;      ;     ;   ;   ; 
;  ;    ;   ;;;;     ;   ;   ; 
;  ;    ;  ;   ;     ;   ;   ; 
;  ;    ;  ;   ;     ;   ;   ; 
;  ;    ;   ;;;;   ;;;;; ;   ; 
;                              
;                              
;                              

;;; Main Window

(define (main-window)
  (define frame (new frame% [label "Save and Load Files"]))

  (define editor #f)
  
  (define name (new text-field% [parent frame] [label "Sprite Name:"]))
  (define width (new text-field% [parent frame] [label "Width"]))
  (define height (new text-field% [parent frame] [label "Height"]))
  (new button% [parent frame] [label "Create New"]
       [callback (lambda (button event)
                   (set! editor (new editor%
                                     [name (send name get-value)]
                                     [width (string->number (send width get-value))]
                                     [height (string->number (send height get-value))]))
                   (gui-editor editor))])
  (new button% [parent frame] [label "Load"]
       [callback (lambda (button event)
                   (set! editor (load (send name get-value)))
                   (gui-editor editor))])
  (new button% [parent frame] [label "Save"]
       [callback (lambda (button event)
                   (if editor (save editor) (void)))])
  (new button% [parent frame] [label "Create PNGs"]
       [callback (lambda (button event)
                   (send editor make-pngs))])
  (send frame show #t)
  (lambda () editor))

(define auto-save (main-window))

