#lang racket/gui
(require 2htdp/image)
(require math/array)

;;Bitmap objects that will eventually be in a hash table
(define initialize (make-object bitmap% "blank.bmp"))
(define path (make-object bitmap% "path.png"))
(define water (make-object bitmap% "water.png"))
(define grass (make-object bitmap% "grass.png"))

;;The main window (mainWin)
(define mainWin (instantiate frame%("Map Generator")))

;;Map class containing overridden canvas on-paint class
(define theMap%
  (class canvas%
    (override on-paint)
    (define on-paint
      (lambda()(send (send theMap get-dc) draw-bitmap initialize 0 0)))
    (super-instantiate())))

;;Create a 400x400px canvas(theMap) inside mainWin
(define theMap (new theMap% (parent mainWin)
                  (min-width 400)
                  (min-height 400)))

;;Generate Button
(new button% [parent mainWin]
             [label "Generate"]
             ;;Button click calls GenerateMap
             [callback (lambda (button event)
                         (GenerateMap))])

;;Procedure for displaying the tiles
(define (printmap theTile xpos ypos)
    (send (send theMap get-dc) draw-bitmap theTile xpos ypos))

;;10x10 array for tiles(may be a dynamic array in the future)
(define arrayMap
    (array #[#[grass water water water grass grass grass grass grass grass]
             #[grass water water water path  path  path  grass grass grass]
             #[grass water water water path  grass path  grass grass grass]
             #[grass grass grass grass path  grass path  path  grass grass]
             #[grass grass grass path  path  grass grass path  grass grass]
             #[grass grass grass path  grass grass grass path  grass grass]
             #[grass grass grass path  grass grass grass path  grass grass]
             #[path  path  path  path  grass grass grass path  path  path]
             #[grass grass grass grass grass grass grass grass grass grass]
             #[grass grass grass grass grass grass grass grass grass grass]]))

;;Vector for array
(define ArrIndex
  (vector 0 0))

;;Increment Vector(There's probably a better way to do this)
(define (incArrIndex)
    ;;Max vector; reset back to 0 0
    (if (and (equal? (vector-ref ArrIndex 0) 9)
             (equal? (vector-ref ArrIndex 1) 9))
        (begin
              (vector-set! ArrIndex 1 0)
              (vector-set! ArrIndex 0 0))
        ;;Increment vector position 0 if position 1 is equal to 9
        (if (equal? (vector-ref ArrIndex 1) 9)
            (begin
              (vector-set! ArrIndex 0 (+ (vector-ref ArrIndex 0) 1))
              (vector-set! ArrIndex 1 0))
            ;;Else, increment vector position 1
            (begin
              (vector-set! ArrIndex 1 (+ (vector-ref ArrIndex 1) 1))))))

;; Iterates through the arrayMap, calls printmap, and increments xpos/ypos by 40px(size of tiles)
(define (GenerateMap)
  (define (GenerateMapiter xpos ypos)
    (begin
      (printmap (array-ref arrayMap ArrIndex) xpos ypos)
      (incArrIndex)
      ;; Max position condition
      (if (and (equal? xpos 360) (equal? ypos 360)) (display "Done Generating \n")
          ;;Increment ypos by 40 if xpos is equal to 360; set xpos to 0
          (if (equal? xpos 360)
              (GenerateMapiter 0 (+ ypos 40))
              ;;Else, increment xpos by 40
              (GenerateMapiter (+ xpos 40) ypos)))))
  (GenerateMapiter 0 0))

(send mainWin show #t)
