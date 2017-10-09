#lang racket/base

(require racket/class
         racket/snip
         racket/format
         "rsound.rkt"
         (only-in "util.rkt" ding))
   
(provide rsound-snip%
         (rename-out [rsound-snip-class snip-class]))
   
(define rsound-snip%
  (class snip%
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (init-field [size 20.0])

    (define width 30)
    (define height 20)
    (super-new)
    (set-snipclass rsound-snip-class)
    (send (get-the-snip-class-list) add rsound-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
      
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (maybe-set-box! w (+ 2.0 width))
      (maybe-set-box! h (+ 2.0 height))
      (maybe-set-box! descent 1.0)
      (maybe-set-box! space 1.0)
      (maybe-set-box! lspace 1.0)
      (maybe-set-box! rspace 1.0))
      
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-rectangle (+ x 1.0) (+ y 1.0) width height))
      
    (define/override (copy)
      (new rsound-snip%))
      
    (define/override (write f)
      (send f put size))
      
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down?)
        (play ding)
        #;((set! size (+ 1.0 size))
           (define admin (get-admin))
           (when admin
             (send admin resized this #t)))))))
   
  (define circle-snip-class%
    (class snip-class%
      (inherit set-classname)
      
      (super-new)
      (set-classname (~s '((lib "rsound-snip.rkt" "rsound")
                           (lib "wxme-rsound-snip.rkt" "rsound"))))
      
      (define/override (read f)
        (define size-b (box 0.0))
        (send f get size-b)
        (new rsound-snip% [size (unbox size-b)]))))
   
  (define rsound-snip-class (new circle-snip-class%))

(new rsound-snip%)
   