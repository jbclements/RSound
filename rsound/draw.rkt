#lang racket/base


(require "rsound.rkt"
         "common.rkt"
         racket/gui
         racket/class
         math/array)

(provide rs-draw
         vector-display-frame
         vector-draw/mag/phase ;; undocumented
         vector-draw/log-mag/phase ;; undocumented
         vector-pair-draw/magnitude
         vector-draw/real/imag
         ffts-draw
         rsound/left-1-fft-draw
         rsound-fft-draw
         ;; for testing
         interpolate
         rasterize-column
         abs-max-from
         phase)

;; for scaling, use something nonzero if the maxes are zero. 
;; it doesn't matter what number we use, because the displayed
;; value will be zero anyway.
(define (make-nonzero n)
  (cond [(or (= n 0) (= n 0.0)) 1.0]
        [else n]))

;; how frequently can the window be redrawn? (in ms)
;; this prevents total bog-down when scrolling 
;; side-to-side in a sound.
(define MIN-REDRAW-INTERVAL 500.0)

;; make-sound-drawing-callback 
;; given a function that gets a sample from the left channel, a function that 
;; gets a sample from the right channel, the number of frames, and ... er ... two things that 
;; are going to go away, produce a function to be used as a drawer.
(define (make-sound-drawing-callback left-getter right-getter vec-len common-scale?)
  (define last-draw-time-box (box (- (current-inexact-milliseconds) MIN-REDRAW-INTERVAL)))
  (define left-actual-max (abs-max-from left-getter vec-len))
  (define right-actual-max (abs-max-from right-getter vec-len))
  (define common-actual-max (max left-actual-max right-actual-max))
  (define left-display-max (make-nonzero
                            (if common-scale? common-actual-max left-actual-max)))
  (define right-display-max (make-nonzero
                            (if common-scale? common-actual-max right-actual-max)))
  (lambda (canvas dc)
    (cond 
      #;[(< (- (current-inexact-milliseconds) (unbox last-draw-time-box)) MIN-REDRAW-INTERVAL)
       #f]
      [else
       (set-box! last-draw-time-box (current-inexact-milliseconds))
       ;; the x position on the virtual canvas:
       (define-values (view-start-x _1) (send canvas get-view-start))
       ;; the width in pixels of the area to be drawn:
       (define-values (client-width _2) (send canvas get-client-size))
       ;; the total width of the virtual canvas:
       (define-values (virtual-canvas-width _3) (send canvas get-virtual-size))
       (define frames-per-pixel (/ vec-len virtual-canvas-width))
       (define data-left (floor (* frames-per-pixel view-start-x)))
       (define frames (floor (* frames-per-pixel client-width)))
       ;; because of canvas resizing or zooming, the window may extend 
       ;; beyond the edge of the sound. Stop at the last real frame.
       (define proposed-data-right (+ data-left frames))
       (define actual-data-right (min vec-len proposed-data-right))
       (define stop-pixel (- (/ actual-data-right frames-per-pixel) view-start-x))
       (let* ([h (- (send canvas get-height) 1)]
              [half-h (floor (/ h 2))]
              [h-scale (/ (- frames 1) (- client-width 1))]
              [v-scale-left (/ (/ half-h 2) left-display-max)]
              [v-scale-right (/ (/ half-h 2) right-display-max)]
              [upper-centerline (* 1/2 half-h)]
              [lower-centerline (* 3/2 half-h)]
              [offset-left-getter (lambda (i) (left-getter (+ i data-left)))]
              [offset-right-getter (lambda (i) (right-getter (+ i data-left)))])
         ;; basically, this is a rasterization problem.
         ;; the very left and right edges are special cases.
         ;; ... in fact, I'll just skip them for now :)
         (for ([i (in-range 1 (- stop-pixel 1))])
           (let ([raster-left (* h-scale (- i 1/2))]
                 [raster-right (* h-scale (+ i 1/2))])
             (let*-values ([(left-min left-max) 
                            (rasterize-column offset-left-getter
                                              raster-left raster-right)]
                           [(right-min right-max) 
                            (rasterize-column offset-right-getter
                                              raster-left
                                              raster-right)])
               (define (num->pixel/left n)
                 (inexact->exact (floor (- upper-centerline (* v-scale-left n)))))
               (define (num->pixel/right n)
                 (inexact->exact (floor (- lower-centerline (* v-scale-right n)))))
               (send dc draw-line
                     (+ view-start-x i) (num->pixel/left left-max)
                     (+ view-start-x i) (num->pixel/left left-min))
               (send dc draw-line
                     (+ view-start-x i) (num->pixel/right right-max)
                     (+ view-start-x i) (num->pixel/right right-min)))))
         #f)])))



;; max-from : (nat -> number) nat -> number
;; find the largest absolute value among the samples in [0,limit)
(define (abs-max-from getter limit)
  (for/fold ([abs-max 0])
    ([i (in-range limit)])
    (max (abs (getter i)) abs-max)))

;; rasterize-column: return the min and max points that the sampled line 
;; reaches in the interval defined by the left and right edge.
;; (nat -> number) number number -> (values number number)
(define (rasterize-column getter left-edge right-edge)
  (let* ([left-edge-left-value (interpolate getter left-edge)]
         [left-edge-right-value (interpolate getter right-edge)]
         
         [in-between-left-values (for/list ([i (in-range 
                                                (ceiling left-edge)
                                                (+ 1 (floor right-edge)))])
                                   (getter i))]
         
         [all-vals (cons left-edge-left-value
                         (cons left-edge-right-value in-between-left-values))]
         
         [left-min (apply min all-vals)]
         [left-max (apply max all-vals)])
      (values left-min left-max)))


;; where does the interpolated line between samples cross a vertical line?
(define (interpolate get-sample n)
  (let* ([fl (floor n)]
         [frac (- n fl)])
    (+ (* (- 1 frac) (get-sample fl)) (* frac (get-sample (+ fl 1))))))

;; SOUND-CANVAS%
;;
;; the canvas that draws a sound

(define sound-canvas%
  (class canvas%
    (init-field len)
    (init-field frame-num-text)
    (init-field y-value-text)
    (init-field left-getter)
    (init-field right-getter)
    (init-field frames-per-pixel)
    
    ;; given a number of frames per pixel, compute the required
    ;; virtual canvas width
    (define (fpp->virtual-width frames-per-pixel)
      (ceiling (/ len frames-per-pixel)))
    
    (unless (positive-integer? len)
      (raise-argument-error 'sound-canvas-init
                            "positive integer" 0 len))
    (unless (< 0 frames-per-pixel)
      (raise-argument-error 'sound-canvas-init
                            "positive number" 0 frames-per-pixel))
    (unless (dimension-integer?
             (fpp->virtual-width frames-per-pixel))
      (raise-argument-error 'sound-canvas-init
                            "number implying legal canvas width"
                            0 frames-per-pixel))
    
    (define cur-mouse-x 0)
    
    (inherit get-width get-height get-parent init-auto-scrollbars
             get-view-start get-client-size get-virtual-size)
    
    (define (get-client-width)
      (define-values (w _) (get-client-size))
      w)
    
    (define/override (on-char evt)
      (define key-code (send evt get-key-code))
      
      (define client-width (get-client-width))
      (define-values (view-start-x _1) (get-view-start))
      (define data-left (floor (* frames-per-pixel view-start-x)))
      (define frames (floor (* frames-per-pixel client-width)))
      ;; given an x coordinate, return the corresponding frame
      (define (pixel->frame x)
          (+ data-left (floor (* frames-per-pixel x))))
      
      (match key-code
        [#\+ (set-frames-per-pixel! (/ frames-per-pixel 2))]
        [#\- (set-frames-per-pixel! (* frames-per-pixel 2))]
        [other #f]))
    
    (define/override (on-event evt)
      (set! cur-mouse-x (send evt get-x))
      (define client-width (get-client-width))
      (define-values (view-start-x _1) (get-view-start))
      (define data-left (floor (* frames-per-pixel view-start-x)))
      ;; given an x coordinate, return the corresponding frame
      (define (pixel->frame x)
          (+ data-left (floor (* frames-per-pixel x))))
      
      (define x (min (max 0 (send evt get-x)) (- (get-client-width) 1)))
      (define scaled-x (pixel->frame x))
      (define y (send evt get-y))
      (define y-val 
        (cond [(< scaled-x len)
               (format-sample
                (if (> y (/ (get-height) 2))
                    (right-getter scaled-x)
                    (left-getter scaled-x)))]
              [else "undefined"]))
      (define frame-num-str
        (cond [(< scaled-x len) scaled-x]
              [else "undefined"]))
      (send frame-num-text begin-edit-sequence #f)
      (send frame-num-text erase)
      (send frame-num-text insert
            (format "frame #: ~a" frame-num-str))
      (send frame-num-text end-edit-sequence)
      (send y-value-text begin-edit-sequence #f)
      (send y-value-text erase)
      (send y-value-text insert 
            (format "y value: ~a" y-val))
      (send y-value-text end-edit-sequence))
    
    ;; change the frames-per-pixel (and reset the virtual width)
    (define (set-frames-per-pixel! fpp)
      (unless (< 0 fpp)
        (raise-argument-error 'set-frames-per-pixel!
                              "positive number" 0 fpp))
      (define virtual-width (fpp->virtual-width fpp))
      (cond [(dimension-integer? virtual-width)
             (set! frames-per-pixel fpp)
             ;; it would be lovely if the position of the 
             ;; scroll-bar were set correctly here:
             (init-auto-scrollbars virtual-width #f 0.0 0.0)]
            [else
             (message-box 
              "Too Much Zoom!"
              (string-append
               "Zooming in to this level requires a virtual " 
               "canvas size that the platform can't handle. "
               "To zoom in further, cut the source data into "
               "smaller pieces (e.g., using (clip ...))."))]))
    
    (super-new)
    ;; this shouldn't fail, we checked it up above:
    (init-auto-scrollbars (fpp->virtual-width frames-per-pixel)
                          #f 0.0 0.0)
    ))


;; given ... a bunch of stuff, create a new window for displaying vector data
;; and show it.
(define (vector-display-frame title left-getter right-getter len width height data-left
                      data-right common-scale?)
  (let* ([f (new frame% [label title] [width width] [height height])]
         [tx (new text%)]
         [ty (new text%)]
         [c (new sound-canvas%
                 [parent f]
                 [paint-callback 
                  (make-sound-drawing-callback left-getter right-getter len common-scale?)]
                 [len len]
                 [frame-num-text tx]
                 [y-value-text   ty]
                 [left-getter left-getter]
                 [right-getter right-getter]
                 [style '(hscroll)]
                 [frames-per-pixel (/ len width)])]
         [ecx (new editor-canvas%
                   [parent f]
                   [editor tx]
                   [style '(no-border no-hscroll no-vscroll)]
                   [stretchable-width #t]
                   [stretchable-height #f]
                   [horizontal-inset 1]
                   [vertical-inset 1]
                   [min-width 50]
                   [min-height 20])]
         [ecy (new editor-canvas%
                   [parent f]
                   [editor ty]
                   [style '(no-border no-hscroll no-vscroll)]
                   [stretchable-width #t]
                   [stretchable-height #f]
                   [horizontal-inset 1]
                   [vertical-inset 1]
                   [min-width 50]
                   [min-height 20])])
    (send f show #t)))

(define (vector-pair-draw/magnitude left-vec right-vec 
                                    #:title [title "magnitude of vector"]
                                    #:width [width 800] #:height [height 230])
  (unless (= (vector-length left-vec)
             (vector-length right-vec))
    (error 'vector-pair-draw/magnitude
           "expected two vectors of the same length, got ~s and ~s" 
           (vector-length left-vec)
           (vector-length right-vec)))
  (vector-display-frame title 
                (lambda (i) (magnitude (vector-ref left-vec i)))
                (lambda (i) (magnitude (vector-ref right-vec i)))
                (vector-length left-vec)
                width
                height
                0
                (vector-length left-vec)
                #f))

;; draw a complex vector, breaking it up into real and imaginary parts
(define (vector-draw/real/imag arr #:title [title "real and imaginary parts"]
                               #:width [width 800] #:height [height 230])
  (unless (= 1 (array-dims arr))
    (raise-argument-error 'vector-draw/real-imag "array of one dimension"
                          0 arr))
  (vector-display-frame title
                        (lambda (i) (real-part (array-ref arr (vector i))))
                        (lambda (i) (imag-part (array-ref arr (vector i))))
                        (array-size arr)
                        width
                        height
                        0
                        (array-size arr)
                        #f))

;; draw a complex vector, breaking it up into magnitude and phase
(define (vector-draw/mag/phase arr #:title [title "magnitude and phase"] #:width [width 800] #:height [height 200])
  (unless (= 1 (array-dims arr))
    (raise-argument-error 'vector-draw/mag/phase "array of one dimension"
                          0 arr))
  (vector-display-frame title
                        (lambda (i) (magnitude (array-ref arr (vector i))))
                        (lambda (i) (phase (array-ref arr (vector i))))
                        (array-size arr)
                        width
                        height
                        0
                        (array-size arr)
                        #f))

;; draw a complex vector as a set of log-magnitudes and phases
(define (vector-draw/log-mag/phase arr #:title [title "log magnitude and phase"] #:width [width 800] #:height [height 200])
  (unless (= 1 (array-dims arr))
    (raise-argument-error 'vector-draw/log-mag/phase "array of one dimension"
                          0 arr))
  (vector-display-frame title
                (lambda (i) (log (magnitude (array-ref arr (vector i)))))
                (lambda (i) (phase (array-ref arr (vector i))))
                (array-size arr)
                width
                height
                0
                (array-size arr)
                #f))


;; draw a sound
(define (rs-draw sound #:title [title "picture of sound"] 
                     #:width [width 800] #:height [height 230])
  (vector-display-frame title
                (lambda (i) (/ (rs-ith/left/s16 sound i) s16max))
                (lambda (i) (/ (rs-ith/right/s16 sound i) s16max))
                (rs-frames sound)
                width
                height
                0
                (rs-frames sound)
                #t))

;; FFTS


;; make-fft-drawing-callback : draw an FFT picture. Assumes 0db = s16max * fft-points / 2
;; (listof (fcarrayof complex?)) (listof (fcarrayof complex?)) -> canvas dc -> void
(define (make-fft-drawing-callback left-ffts right-ffts fft-show-points)
  (unless (= (length left-ffts) (length right-ffts))
    (raise-argument-error
     'make-fft-drawing-callback 
     "lists of equal length" 1 left-ffts right-ffts fft-show-points))
  (unless (andmap (lambda (a) (= (array-dims a) 1)) left-ffts)
    (raise-argument-error
     'make-fft-drawing-callback
     "list of arrays of one dimension"
     0 left-ffts right-ffts fft-show-points))
  (unless (andmap (lambda (a) (= (array-dims a) 1)) right-ffts)
    (raise-argument-error
     'make-fft-drawing-callback
     "list of arrays of one dimension"
     1 left-ffts right-ffts fft-show-points))
  (when (empty? left-ffts)
    (raise-argument-error
     'make-fft-drawing-callback
     "nonempty list"
     0 left-ffts right-ffts fft-show-points))
  (unless (all-equal? (map array-shape (append left-ffts right-ffts)))
    (raise-argument-error
     'make-fft-drawing-callback
     "transforms with same numbers of points"
     0 left-ffts right-ffts fft-show-points))
  (unless (= (modulo (array-size (first left-ffts)) 2) 0)
    (raise-argument-error
     'make-fft-drawing-callback
     "transforms with an even number of points"
     0 left-ffts right-ffts fft-show-points))
  (lambda (canvas dc)
    (let* ([h (send canvas get-height)]
           [half-h (/ h 2)]
           [w (send canvas get-width)]
           [windows (length left-ffts)]
           [fft-max-points (add1 (/ (array-size (first left-ffts)) 2))]
           [fft-points (if fft-show-points
                           (min fft-show-points fft-max-points)
                           fft-max-points)]
           [h-scale (/ windows w)]
           [v-scale (/ fft-points half-h)]
           [fft-max (* s16max (array-size (first left-ffts)) 1/2)])
      (for ([i (in-range 0 windows)]
            [left-fft (in-list left-ffts)]
            [right-fft (in-list right-ffts)])
        (let* ([win-left (round (/ i h-scale))]
               [win-right (round (/ (+ i 1) h-scale))])
          (send dc set-pen "black" 0 'transparent)
          (for ([j (in-range 0 fft-points)])
            (let* ([win-bottom (round (/ j v-scale))]
                   [win-top (round (/ (+ j 1) v-scale))])
              (define (draw-fft-rect top bottom magnitude)
                ;; 0.1 is assumed to be very very quiet.
                (let* ([decibels (* 10 (/ (log (/ (max 0.1 magnitude) fft-max))
                                          (log 10)))]
                       ;; let's set -30db as the limit of gray...
                       [gray-level (min 255 (- (inexact->exact 
                                                (round (* 255/30 decibels)))))])
                  (send dc set-brush (make-object color% gray-level gray-level
                                       gray-level)
                        'solid)
                  (send dc draw-rectangle win-left top (- win-right win-left)
                        (- bottom top))))
              (draw-fft-rect (- half-h win-top) (- half-h win-bottom)
                             (magnitude (array-ref left-fft (vector j))))
              (draw-fft-rect (- h win-top) (- h win-bottom)
                             (magnitude (array-ref right-fft (vector j)))))))))))

(define unequal-lengths-msg
  "left and right channels must have the same number of fft windows, given ~s and ~s")
(define uneven-vec-lengths-msg
  "ffts must have an even number of points. That's just plain confusing.")

(define fft-canvas%
  (class canvas%
    (init-field len)
    (init-field frame-num-text)
    (init-field data-len)
    
    (define data-right data-len)
    (define data-left 0)
    
    (define data-window-width (- data-right data-left))
    
    (inherit get-width get-height get-parent)
    
    (define/override (on-event evt)
      (let* ([x (min (max 0 (send evt get-x)) (- (get-width) 1))]
             [scaled-x (pixel->frame x)])
        (send frame-num-text begin-edit-sequence #f)
        (send frame-num-text erase)
        (send frame-num-text insert 
              (format "frame #: ~a" (number->string scaled-x)))
        (send frame-num-text end-edit-sequence)))
    
    ;; given an x coordinate, return the corresponding frame
    (define (pixel->frame x)
      (+ data-left (floor (* data-window-width (/ x (get-width))))))
    
    (super-new)))

(define (ffts-draw left-ffts right-ffts len fft-show-points 
                   #:title [title "Fourier Transforms"]
                   #:width [width 800]
                   #:height [height 200])
  (let* ([f (new frame% [label title] [width width] [height height])]
         [tx (new text%)]
         [ty (new text%)]
         [c (new fft-canvas%
                 [parent f]
                 [paint-callback 
                  (make-fft-drawing-callback left-ffts
                                             right-ffts
                                             fft-show-points)]
                 [len len]
                 [frame-num-text tx]
                 [data-len len])])
    (send f show #t)))


(define (rsound-fft-draw rsound
                         #:title [title "Fourier Transforms"]
                         #:width [width 800]
                         #:height [height 200]
                         #:zoom-freq [zoom-freq #f]
                         #:window-size [window-size 2048])
  (define windows (floor (/ (rs-frames rsound) window-size)))
  (when (= windows 0)
    (error 'rsound-fft-draw not-enough-frames-msg
           (rs-frames rsound)
           window-size))
  (define (ffts-from-getter getter)
    (for/list ([i (in-range windows)])
      (let* ([s (* window-size i)]
             [e (* window-size (add1 i))]
             [v (build-array (vector (- e s)) 
                             (lambda (i) (getter rsound (+ s (vector-ref i 0)))))])
        (array-fft v))))
  
  (ffts-draw (ffts-from-getter rs-ith/left/s16) 
             (ffts-from-getter rs-ith/right/s16) 
             (* windows window-size)
             (if zoom-freq
                 (round (* window-size 
                           (/ zoom-freq (rsound-sample-rate rsound))))
                 (add1 (round (* window-size 1/2))))
             #:width width
             #:height height
             #:title title))

(define (rsound/left-1-fft-draw rsound
                                #:title [title "Fourier Transforms"]
                                #:width [width 800]
                                #:height [height 200])
  
  (define window-size (rs-frames rsound))
  (define vec-as-array (build-array (vector window-size) (lambda (i) (rs-ith/left/s16 rsound (vector-ref i 0)))))
  (define the-fft (array-fft vec-as-array))
  (vector-draw/mag/phase the-fft
                         #:title title
                         #:width width
                         #:height height))

(define not-enough-frames-msg 
  (string-append "this sound has ~s frames, fewer than the ~s needed for "
                 "one fft window. Use a longer sound or shorten the window."))

(define (all-equal? l)
  (cond [(empty? l) #t]
        [(empty? (rest l)) #t]
        [else (and (equal? (first l) (first (rest l)))
                   (all-equal? (rest l)))]))

;; return the phase of a complex number.  For 0+0i, return 0.
(define (phase cplx)
  (if (= cplx 0+0i)
    0.0
    (let ([phase (/ (log (/ cplx (magnitude cplx))) +i)])
      (unless (< (abs (imag-part phase)) 1e-4)
        (error 'phase imaginary-phase!?-msg phase cplx))
      (real-part phase))))

(define imaginary-phase!?-msg
  "oh dear; phase should be a real number, got ~s for complex number ~s")


(define (format-sample n)
  (real->decimal-string n digits-to-print))

(define digits-to-print 4)