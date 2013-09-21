#lang racket/base

;; ooh... looks like I was starting to put together a snip for rsounds.... I should finish this!


(require racket/gui)

(define red-arrow-bitmap
  (make-object bitmap% (build-path (collection-path "icons") "red-arrow.bmp") 'bmp))

(unless (send red-arrow-bitmap ok?)
  (error 'red-arrow-bitmap "unable to load red-arrow bitmap"))

(define rsound-snip-class%
  (class snip-class%
    (override read)
    
    (define (read s)
      (let ([size-box (box 0)])
        (send s get size-box)
        (make-object rsound-snip% 100)))
    
    (super-instantiate ())))

(define rsound-snipclass
  (make-object rsound-snip-class%))


(send* rsound-snipclass
  (set-version 1)
  (set-classname (format "~s" `(lib "vertical-separator-snip.ss" "stepper" "private"))))

(send (get-the-snip-class-list) add rsound-snipclass)

(define rsound-snip%
  (class snip%
    (inherit get-style set-snipclass set-flags get-flags get-admin)
    (public set-height!)
    (override write copy get-extent draw)
    
    (init-field height)
    
    
    (define bitmap-width 15.0)
    (define left-white 0.0)
    (define right-white 3.0)
    (define bitmap-height 10.0)
    
    (define (set-height! x) 
      (set! height (max x bitmap-height)))
    
    (define (write s) 
      (send s put (char->integer #\r)))        ; this can't be right...?
    
    (define (copy)
      (let ([s (make-object rsound-snip% height)])
        (send s set-style (get-style))
        s))
    
    (define (get-extent dc x y w-box h-box descent-box space-box lspace-box rspace-box)
      (for-each (lambda (box) (unless (not box) (set-box! box 0)))
                (list descent-box space-box lspace-box rspace-box))
      (unless (not w-box)
        (set-box! w-box (+ left-white right-white bitmap-width)))
      (unless (not h-box)
        (set-box! h-box height)))
    
    (define (draw dc x y left top right bottom dx dy draw-caret)
      (let ([y-offset (round (/ (- height bitmap-height) 2))]
            [x-offset left-white])
        (send dc draw-bitmap red-arrow-bitmap (+ x x-offset) (+ y y-offset))))
    
    (super-instantiate ())
    (set-snipclass rsound-snipclass)))