#lang racket


(require "rsound.rkt"
         racket/gui
         racket/class
         "fft.rkt")

(provide rs-draw
         vectors-draw
         vector-draw/mag/phase ;; undocumented
         vector-draw/log-mag/phase ;; undocumented
         vector-pair-draw/magnitude
         vector-draw/real/imag
         ffts-draw
         rsound-fft-draw
         ;; for testing
         interpolate
         rasterize-column
         abs-max-from
         phase)

;; make-sound-drawing-callback 
;;  : (nat -> real) (nat->real) nat nat nat -> canvas dc -> void
(define (make-sound-drawing-callback left-getter right-getter vec-len data-left
                                     data-right)
  (let ([sound-max (max (abs-max-from left-getter vec-len)
                        (abs-max-from right-getter vec-len))])
    (unless (<= 0 data-left data-right vec-len)
      (error 
       'make-sound-drawing-callback 
       "must have 0 <= data-left <= data-right <= frames, given 0 <= ~s <= ~s <= ~s"
             data-left data-right vec-len))
    (when (= sound-max 0.0)
      (error
       'make-sound-drawing-callback
       "max value is 0.0, vectors are uniformly 0."))
    (lambda (canvas dc)
      (let* ([h (- (send canvas get-height) 1)]
             [half-h (floor (/ h 2))]
             [w (send canvas get-width)]
             [frames (- data-right data-left)]
             [h-scale (/ (- frames 1) (- w 1))]
             [v-scale (/ (/ half-h 2) sound-max)]
             [upper-centerline (* 1/2 half-h)]
             [lower-centerline (* 3/2 half-h)]
             [offset-left-getter (lambda (i) (left-getter (+ i data-left)))]
             [offset-right-getter (lambda (i) (right-getter (+ i data-left)))])
        ;; basically, this is a rasterization problem.
        ;; the very left and right edges are special cases.
        ;; ... in fact, I'll just skip them for now :)
        (for ([i (in-range 1 (- w 1))])
          (let ([raster-left (* h-scale (- i 1/2))]
                [raster-right (* h-scale (+ i 1/2))])
            (let*-values ([(left-min left-max) 
                           (rasterize-column offset-left-getter
                                             raster-left raster-right)]
                          [(right-min right-max) 
                           (rasterize-column offset-right-getter
                                             raster-left
                                             raster-right)])
              (define (num->pixel centerline n)
                (inexact->exact (floor (- centerline (* v-scale n)))))
              (send dc draw-line
                    i (num->pixel upper-centerline left-max)
                    i (num->pixel upper-centerline left-min))
              (send dc draw-line
                    i (num->pixel lower-centerline right-max)
                    i (num->pixel lower-centerline right-min)))))))))



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


(define sound-canvas%
  (class canvas%
    (init-field len)
    (init-field frame-num-text)
    (init-field y-value-text)
    (init-field left-getter)
    (init-field right-getter)
    ;; what portion of the data is shown in the window?
    (init-field data-left)
    (init-field data-right)
    
    (define data-window-width (- data-right data-left))
    
    (inherit get-width get-height get-parent)
    
    (define/override (on-event evt)
      (cond [(send evt button-down?)
             (let* ([x (min (max 0 (send evt get-x)) (- (get-width) 1))]
                    [scaled-x (pixel->frame x)]
                    [data-middle (round (/ (+ data-left data-right) 2))])
               (match-let ([(list new-left new-right) 
                            (cond [(< x (/ (get-width) 2)) (list data-left data-middle)]
                                  [else                    (list data-middle data-right)])])
                 (cond [(< data-left (- data-right 2))
                        (vectors-draw "zoomed" left-getter right-getter len (send (get-parent) get-width) 
                                      (send (get-parent) get-height) new-left new-right)]
                       [else
                        ;; buffer too short to zoom; just ignore the click.
                        (void)])))]
            [else
             (let* ([x (min (max 0 (send evt get-x)) (- (get-width) 1))]
                    [scaled-x (pixel->frame x)]
                    [y (send evt get-y)]
                    [y-val (if (> y (/ (get-height) 2))
                               (right-getter scaled-x)
                               (left-getter scaled-x))])
               (send frame-num-text begin-edit-sequence #f)
               (send frame-num-text erase)
               (send frame-num-text insert (format "frame #: ~a" (number->string scaled-x)))
               (send frame-num-text end-edit-sequence)
               (send y-value-text begin-edit-sequence #f)
               (send y-value-text erase)
               (send y-value-text insert 
                     (format "y value: ~a" 
                             (format-sample
                              y-val)))
               (send y-value-text end-edit-sequence))]))
    
    ;; given an x coordinate, return the corresponding frame
    (define (pixel->frame x)
      (+ data-left (floor (* data-window-width (/ x (get-width))))))
    
    (super-new)))


(define (vectors-draw title left-getter right-getter len width height data-left
                      data-right)
  (let* ([f (new frame% [label title] [width width] [height height])]
         [tx (new text%)]
         [ty (new text%)]
         [c (new sound-canvas%
                 [parent f]
                 [paint-callback 
                  (make-sound-drawing-callback left-getter right-getter
                                               len data-left data-right)]
                 [len len]
                 [frame-num-text tx]
                 [y-value-text   ty]
                 [left-getter left-getter]
                 [right-getter right-getter]
                 [data-left data-left]
                 [data-right data-right])]
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
                                    #:width [width 800] #:height [height 200])
  (unless (= (vector-length left-vec)
             (vector-length right-vec))
    (error 'vector-pair-draw/magnitude
           "expected two vectors of the same length, got ~s and ~s" 
           (vector-length left-vec)
           (vector-length right-vec)))
  (vectors-draw title 
                (lambda (i) (magnitude (vector-ref left-vec i)))
                (lambda (i) (magnitude (vector-ref right-vec i)))
                (vector-length left-vec)
                width
                height
                0
                (vector-length left-vec)))

(define (vector-draw/real/imag vec #:title [title "real and imaginary parts"]
                               #:width [width 800] #:height [height 200])
  (vectors-draw title
                (lambda (i) (real-part (vector-ref vec i)))
                (lambda (i) (imag-part (vector-ref vec i)))
                (vector-length vec)
                width
                height
                0
                (vector-length vec)))

(define (rs-draw sound #:title [title "picture of sound"] 
                     #:width [width 800] #:height [height 200])
  (vectors-draw title
                (lambda (i) (rs-ith/left/s16 sound i))
                (lambda (i) (rs-ith/right/s16 sound i))
                (rs-frames sound)
                width
                height
                0
                (rs-frames sound)))

(define (vector-draw/mag/phase vec #:title [title "magnitude and phase"] #:width [width 800] #:height [height 200])
  (vectors-draw title
                (lambda (i) (magnitude (vector-ref vec i)))
                (lambda (i) (phase (vector-ref vec i)))
                (vector-length vec)
                width
                height
                0
                (vector-length vec)))

(define (vector-draw/log-mag/phase vec #:title [title "log magnitude and phase"] #:width [width 800] #:height [height 200])
  (vectors-draw title
                (lambda (i) (log (magnitude (vector-ref vec i))))
                (lambda (i) (phase (vector-ref vec i)))
                (vector-length vec)
                width
                height
                0
                (vector-length vec)))

;; FFTS


;; make-fft-drawing-callback : draw an FFT picture. Assumes 0db = s16max * fft-points / 2
;; (listof (vectorof complex?)) (listof (vectorof complex?)) -> canvas dc -> void
(define (make-fft-drawing-callback left-ffts right-ffts fft-show-points)
  (unless (= (length left-ffts) (length right-ffts))
    (error 'make-fft-drawing-callback 
           unequal-lengths-msg 
           (length left-ffts) (length right-ffts)))
  (when (empty? left-ffts)
    (error 'make-fft-drawing-callback "called with empty lists of ffts"))
  (unless (apply = (map vector-length (append left-ffts right-ffts)))
    (error 'make-fft-drawing-callback 
           "transforms must all have the same number of points, given ~e"
           (map vector-length (append left-ffts right-ffts))))
  (unless (= (modulo (vector-length (first left-ffts)) 2) 0)
    (error 'make-fft-drawing-callback 
           uneven-vec-lengths-msg))
  (lambda (canvas dc)
    (let* ([h (send canvas get-height)]
           [half-h (/ h 2)]
           [w (send canvas get-width)]
           [windows (length left-ffts)]
           [fft-max-points (add1 (/ (vector-length (first left-ffts)) 2))]
           [fft-points (if fft-show-points
                           (min fft-show-points fft-max-points)
                           fft-max-points)]
           [h-scale (/ windows w)]
           [v-scale (/ fft-points half-h)]
           [fft-max (* s16max (vector-length (first left-ffts)) 1/2)])
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
                             (magnitude (vector-ref left-fft j)))
              (draw-fft-rect (- h win-top) (- h win-bottom)
                             (magnitude (vector-ref right-fft j))))))))))

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
  
  (define window-size 2048)
  (define windows (floor (/ (rs-frames rsound) window-size)))
  (when (= windows 0)
    (error 'rsound-fft-draw not-enough-frames-msg
           (rs-frames rsound)
           window-size))
  (define (ffts-from-getter getter)
    (for/list ([i (in-range windows)])
      (let* ([s (* window-size i)]
             [e (* window-size (add1 i))]
             [v (build-vector (- e s) (lambda (i) (getter rsound (+ s i))))])
        (fft-complex-radix2-forward v)
        v)))
  
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

(define not-enough-frames-msg 
  (string-append "this sound has ~s frames, fewer than the ~s needed for "
                 "one fft window. Use a longer sound or shorten the window."))


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
  (real->decimal-string (/ n s16max) digits-to-print))

(define digits-to-print 4)