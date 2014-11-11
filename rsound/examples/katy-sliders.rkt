;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname katy-sliders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rsound)

(define SONG (rs-read "/tmp/partygoing.wav"))
(define SONGLEN (rs-frames SONG))

; Copyright 2012, John Clements (clements@brinckerhoff.org)
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;  http://www.apache.org/licenses/LICENSE-2.0
; 
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

;; TO SHOW TO STUDENTS:
;; - LOCAL
;; - LIST SYNTAX
;; - ERRORS

;; demo of some sliders

;; DATA DEFINITIONS:

;; a world is  
;; (make-ws list-of-components maybe-component number number)
;; interp: 
;;  - cs: the list of components in the world,
;;  - live: the posn of the component currently being dragged,
;;  - W : the width of the scene, and
;;  - H : the height of the scene
(define-struct ws (cs live W H))

;; a list-of-components is either
;; - empty, or
;; - (cons component list-of-components)

;; a component is (make-component vslider posn)
;; interp:
;; - s : the slider (currently, only sliders are allowed)
;; - posn : the location of the upper-left corner of the component
;;          within the global scene.
(define-struct component (s posn))

;; NOTE: the 'posn' structure is already defined.

;; a maybe-component is either
;; - false, or
;; - a posn
;; interp. false -> no slider is live, 
;;  - posn, e.g. (make-posn 30 40) -> the component at 30,40 is live
;; NOTE: if two components have the same x and y, we're in big trouble.

;; a vslider is (make-vslider number number fraction)
;; interp:
;; - W : the width of the slider on the screen
;; - H : the height of the slider on the screen
;; - val : the fraction representing the position of
;;         the slider; 0 is at the bottom, 1 is at the top.
(define-struct vslider (W H val))

;; a fraction is a number between 0.0 and 1.0 inclusive

(define VSLIDER-HANDLE-H 40)
(define SLOT-OFFSET (/ VSLIDER-HANDLE-H 2))

;;;;;;;;;;;;
;;
;;  DRAWING
;;
;;;;;;;;;;;;

;; draw the world
;; world -> image
(define (draw-world ws)
  (begin
    ;; horrible, I hate it, needed for signals:
    (set-box! world-box ws)
    (place-components (ws-cs ws)
                      (empty-scene (ws-W ws) (ws-H ws)))))


;; place-components : list-of-components scene -> scene
;; place all of the given components onto the scene
(define (place-components comps scene)
  (cond [(empty? comps) scene]
        [else (place-components 
               (rest comps)
               (place-component (first comps)
                                scene))]))

;; place-component : component scene -> scene
;; place the image of the component onto the scene
;; (tested indirectly.)
(define (place-component c s)
  (place-image (draw-vslider (component-s c))
               (+ (posn-x (component-posn c)) (/ (component-width c) 2))
               (+ (posn-y (component-posn c)) (/ (component-height c) 2))
               s))


;; draw a vertical slider
;; slider -> image
(define (draw-vslider vs)
  (local
    [(define w (vslider-W vs))
     (define h (vslider-H vs))
     (define slot-len (- h (* 2 SLOT-OFFSET)))
     (define slider-pixels (- (+ slot-len SLOT-OFFSET)
                              (* slot-len (vslider-val vs))))]
    (place-image
     (overlay
      (text (number->string (min 99 (round (* (vslider-val vs) 100))))
            35 
            "solid")
      (rectangle w VSLIDER-HANDLE-H "solid" 
                 (make-color #xE0 #xD0 #x90)))
     (/ w 2) slider-pixels
     (add-line (rectangle w h "solid" "white")
               (/ w 2) SLOT-OFFSET (/ w 2) (- h SLOT-OFFSET) 
               (make-pen "black" 7 "solid" "round" "round")))))
                                                               
                                                              
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  HANDLING MOUSE EVENTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;



;; handle a mouse event;
;; world number number string -> world
(define (meh ws x y event)
  ;; BUTTON-DOWN EVENT:
  (cond [(string=? event "button-down")
         (local
           [(define chosen-component (xy->component (ws-cs ws) x y))]
           ;; missed all components, leave the world state alone:
           (cond [(false? chosen-component) ws]
                 ;; hit one: turn it on:
                 [else
                  (make-ws (components-posn-update
                            (ws-cs ws)
                            chosen-component
                            y)
                           chosen-component
                           (ws-W ws)
                           (ws-H ws))]))]
        ;; BUTTON-UP EVENT:
        [(string=? event "button-up")
         ;; turn the live one off:
         (make-ws (ws-cs ws) #f (ws-W ws) (ws-H ws))]
        ;; OTHER EVENT:
        [else
         (cond [(false? (ws-live ws)) ws]
               [else
                ;; one of them is already live
                (make-ws (components-posn-update (ws-cs ws) (ws-live ws) y)
                         (ws-live ws)
                         (ws-W ws)
                         (ws-H ws))])]))


;; components-posn-update : list-of-components posn number
;; return a list of components with the chosen component updated
;; to the position indicated by the mouse
(define (components-posn-update comps live y)
  (cond [(empty? comps) (error 'components-posn-update
                               "no matching component")]
        [else (cond 
                [(equal? (component-posn (first comps)) live)
                 ;; found it!
                 (cons (component-posn-update (first comps) y)
                       (rest comps))]
                [else ;; keep searching:
                 (cons (first comps)
                       (components-posn-update (rest comps) live y))])]))

;; component-posn-update : component number -> component
;; update the given component to reflect the y position of the mouse
;; (tested indirectly.)
(define (component-posn-update c y)
  (make-component
   (slider-posn-update (component-s c) (- y (posn-y (component-posn c))))
   (component-posn c)))

;; slider-posn-update: slider number -> slider
;; given a vertical slider and a Y mouse position relative to the 
;; slider, update the slider accordingly
(define (slider-posn-update vs ypos)
  (local
    [(define range (- (vslider-H vs)
                      (* 2 SLOT-OFFSET)))
     (define capped (max SLOT-OFFSET 
                         (min (- (vslider-H vs) SLOT-OFFSET)
                              ypos)))
     (define new-posn (- 1.0 
                         (/ (- capped SLOT-OFFSET) range)))]
    (make-vslider (vslider-W vs) (vslider-H vs) new-posn)))

;; component-width : the width of the component
;; [tested indirectly]
(define (component-width c) (vslider-W (component-s c)))

;; component-height : the height of the component
;; [tested indirectly]
(define (component-height c) (vslider-H (component-s c)))

;; determine which component a mouse position is in
;; xy->component : number number -> maybe-component
(define (xy->component components x y)
  (cond [(empty? components) false]
        [else (local [(define fc (first components))]
                (cond [(in-bounding-box? fc x y)
                       (component-posn fc)]
                      [else (xy->component (rest components) x y)]))]))

;; in-bounding-box? : component pixels pixels -> boolean
;; determine whether an x and a y are in the bounding box of a 
;; component.
(define (in-bounding-box? component x y)
  (local [(define p (component-posn component))]
  (and (<= (posn-x p) x (sub1 (+ (posn-x p)
                                 (component-width component))))
       (<= (posn-y p) y (sub1 (+ (posn-y p)
                                 (component-height component)))))))

(define initial-world
  (make-ws (list 
            (make-component (make-vslider 50 400 0.2)
                            (make-posn 50 30))
            (make-component (make-vslider 50 400 0.2)
                            (make-posn 105 30))
            (make-component (make-vslider 50 400 0.2)
                            (make-posn 160 30))
            (make-component (make-vslider 50 400 0.2)
                            (make-posn 215 30))
            (make-component (make-vslider 50 400 0.2)
                            (make-posn 270 30)))
           #f
           375 460))

;;;;;;;
;;
;; SOUND STUFF
;;
;;;;;;


;; fetch the speed from the current world. Ranges from -2 to 2
(define (fetch-speed)
  (- (* 4.0
        (vslider-val 
         (component-s 
          (first (ws-cs (unbox world-box))))))
     2.0))

;; fetch the current delay from the world
(define (fetch-delay)
  (* 20000.0
     (vslider-val 
      (component-s 
       (second (ws-cs (unbox world-box)))))))

;; OUCH! I HATE IT! MUTABLE BOX FOR THE WORLD
(define world-box (box initial-world))

;; increment "old" by "incr", and wrap around if necessary
;; number number number -> number
(define (maybe-wrap old incr len)
  (local [(define new (+ old incr))]
    (cond [(<= len new) (- new len)]
          [(< new 0) (+ len new)]
          [else new])))

;; a network that smoothly moves through a sound.
(define (flexloop len)
  (network (incr)
    [ctr = (maybe-wrap 
            (prev ctr 0.0)
            incr
            len)]))

(signal-play
 (network ()
   [ctr <= (flexloop SONGLEN) (fetch-speed)]
   [delayed-ctr = (maybe-wrap ctr (fetch-delay) SONGLEN)]
   [out = (/ (+ (rs-ith/left SONG (floor ctr))
                (rs-ith/left SONG (floor delayed-ctr)))
             2.0)]))



;;;;;;;;;;;;;;;;
;;
;;   BIG BANG
;;
;;;;;;;;;;;;;;;;




(big-bang initial-world
          [on-mouse meh]
          [to-draw draw-world])



;*******
;
;   TEST CASES
;
;*******


(check-expect
 (draw-vslider 
  (make-vslider 50 200 1.0))
 (place-image
  (overlay 
   (text "99" 35 "black")
   (rectangle 100 40 "solid" (make-color #xE0 #xD0 #x90)))
  25 20
  (add-line (rectangle 50 200 "solid" "white")
            25 20 25 180 
            (make-pen "black" 7 "solid" "round" "round"))))

(check-expect
 (draw-vslider 
  (make-vslider 50 300 0.5))
 (place-image
  (overlay
   (text "50" 35 "black")
  (rectangle 100 40 "solid" (make-color #xE0 #xD0 #x90)))
  25 150
  (add-line (rectangle 50 300 "solid" "white")
            25 20 25 280 
            (make-pen "black" 7 "solid" "round" "round"))))

(check-expect
 (slider-posn-update (make-vslider 50 540 0.2) 145)
 (make-vslider 50 540 3/4))

(check-expect 
 (place-components 
  (list (make-component (make-vslider 30 100 1/2) (make-posn 15 10))
        (make-component (make-vslider 40 80 3/4) (make-posn 45 5)))
  (empty-scene 400 500))
 (place-image (draw-vslider (make-vslider 40 80 3/4))
  65 45
  (place-image (draw-vslider (make-vslider 30 100 1/2))
               30 60
               (empty-scene 400 500))))

(check-expect 
 (components-posn-update 
  (list (make-component (make-vslider 30 100 1/2) (make-posn 15 10))
        (make-component (make-vslider 40 80 3/4) (make-posn 45 5)))
  (make-posn 45 5)
  0)
 (list (make-component (make-vslider 30 100 1/2) (make-posn 15 10))
        (make-component (make-vslider 40 80 1) (make-posn 45 5))))

(define test-component (make-component (make-vslider 30 40 1/2)
                                          (make-posn -50 16)))
(check-expect (in-bounding-box? test-component -51 16) #f)
(check-expect (in-bounding-box? test-component -50 15) #f)
(check-expect (in-bounding-box? test-component -50 16) #t)
(check-expect (in-bounding-box? test-component -21 16) #t)
(check-expect (in-bounding-box? test-component -20 16) #f)
(check-expect (in-bounding-box? test-component -21 55) #t)
(check-expect (in-bounding-box? test-component -21 56) #f)

(define example-components
  (list (make-component (make-vslider 100 150 0.1) (make-posn 30 40))
        (make-component (make-vslider 10 30 0.5) (make-posn 140 20))))

(check-expect (xy->component example-components 0 0) false)
(check-expect (xy->component example-components 30 40) (make-posn 30 40))
(check-expect (xy->component example-components 35 45) (make-posn 30 40))
(check-expect (xy->component example-components 130 45) false)
(check-expect (xy->component example-components 141 45) (make-posn 140 20))



(check-expect 
 (draw-world (make-ws (list
                       (make-component (make-vslider 50 150 0.3)
                                       (make-posn 23 49)))
                      #t
                      500 300))
 (place-image (draw-vslider (make-vslider 50 150 0.3))
              (+ 23 25) (+ 49 75)
              (empty-scene 500 300)))


(check-expect 
 (meh (make-ws (list (make-component (make-vslider 50 540 0.2)
                                     (make-posn 0 0)))
               (make-posn 0 0)
               50 540)
      100 145 "move")
 (make-ws (list (make-component (make-vslider 50 540 3/4)
                                (make-posn 0 0)))
          (make-posn 0 0)
          50 540))

;; ignored move:
(check-expect 
 (meh (make-ws (list 
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0)))
               #f
               50 540)
      100 145 "move")
 (make-ws (list
           (make-component (make-vslider 50 540 1/5) (make-posn 0 0)))
          #f
          50 540))



;; missed click:
(check-expect 
 (meh (make-ws (list 
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0)))
               #f
               50 540)
      100 145 "button-down")
 (make-ws (list 
           (make-component (make-vslider 50 540 1/5)
                           (make-posn 0 0)))
          #f
          50 540))

;; hit click
(check-expect 
 (meh (make-ws (list 
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0)))
               #f
               50 540)
      40 145 "button-down")
 (make-ws  (list 
            (make-component (make-vslider 50 540 3/4) 
                            (make-posn 0 0))) 
           (make-posn 0 0)
           50 540))


;; release:
(check-expect 
 (meh (make-ws (list 
                (make-component (make-vslider 50 540 1/5) 
                                (make-posn 0 0))) #t
               50 540)
      100 145 "button-up")
 (make-ws (list (make-component (make-vslider 50 540 1/5) 
                                (make-posn 0 0))) #f
           50 540))