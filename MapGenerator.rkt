#lang racket/gui
(require 2htdp/image)
(require math/array)

;;Hashtable for different terrain types
(define (makeTable)
  (let ((table (make-hash)))
    (define (lookup key)
      (hash-ref table key))
    (define (insert! key value)
      (hash-set! table key value))
  (define (dispatch m)
    (cond ((eq? m 'lookup) lookup)
          ((eq? m 'insert!) insert!)
          (else (error "Unknown format -- TABLE" m))))
  dispatch))

;;Basic Definitions
(define terrainTable (makeTable))
(define putTerrain (terrainTable 'insert!))
(define getTerrain (terrainTable 'lookup))

;;Data Structure Basis
(define (makeTerrain passable texture)
  (cons passable texture))
(define (getPassable terrain)
  (car (getTerrain terrain)))
(define (getTexture terrain)
  (cdr (getTerrain terrain)))

;;Basic objects
(putTerrain 'default
            (makeTerrain #f (make-object bitmap% "blank.bmp")))
(putTerrain 'grass
            (makeTerrain #t (make-object bitmap% "grass.png")))
(putTerrain 'path
            (makeTerrain #t (make-object bitmap% "path.png")))
(putTerrain 'water
            (makeTerrain #f (make-object bitmap% "water.png")))
(putTerrain 'ice
            (makeTerrain #t (make-object bitmap% "ice.png")))
(putTerrain 'lava
            (makeTerrain #f (make-object bitmap% "lava.png")))

;;The main window (mainWin)
(define mainWin (instantiate frame%("Map Generator")))

;;Map class containing overridden canvas on-paint class
(define theMap%
  (class canvas%
    (override on-paint)
    (define on-paint
      (lambda()(send (send theMap get-dc) draw-bitmap (getTexture 'default) 0 0)))
    (super-instantiate())))

;;Create a 400x400px canvas(theMap) inside mainWin
(define theMap (new theMap% (parent mainWin)
                  (min-width 800)
                  (min-height 800)))

;;Generate Button
(new button% [parent mainWin]
             [label "Generate"]
             ;;Button click resets the map, randomizes it, then displays it.
             [callback (lambda (button event)
                         (begin
                           (Reset)
                           (randomizeArray)
                           (DisplayMap)))])

;;Water check-box
(define watercheck
  (new check-box% [parent mainWin]
                  [label "Water(does nothing)"]
                  [callback (lambda (check-box event)
                            (if(equal?(send watercheck get-value) #t)
                               (display "Generate Water \n")
                               (display "Don't Generate Water \n")))]))

;;Lava check-box
(define lavacheck
  (new check-box% [parent mainWin]
                  [label "Lava(does nothing)"]
                  [callback (lambda (check-box event)
                            (if(equal?(send lavacheck get-value) #t)
                               (display "Generate Lava \n")
                               (display "Don't Generate Lava \n")))]))

;;Procedure for printing
(define (printmap theTile xpos ypos)
    (send (send theMap get-dc) draw-bitmap theTile xpos ypos))

;;20x20 array for tiles(may be a dynamic array in the future)
(define arrayMap null)

(define (Reset)
    (set! arrayMap
       (mutable-array #[ #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     PROCEDURE FOR DISPLAYING THE MAP       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (DisplayMap)
  (begin
    (define ArrIndex (vector 0 0)) ;Used to iterate through the map array
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;INCREMENT VECTOR - This procedure increments the ArrIndex vector by 1 everytime it's called.     ;;
    ;;                   If the procedure is called when the vector is 0:19, it resets the y value     ;;
    ;;                   to 0 and increments the x value resulting in 1:0. If the procedure is called  ;;
    ;;                   when the vector is 19:19, the next incrememnt is out of bounds and the vector ;;
    ;;                   will not be incremented and instead reset back to 0:0.                        ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (incArrIndex)
      (if (and (equal? (vector-ref ArrIndex 0) 19) ;If Vector is 19:19 reset back to 0 0
               (equal? (vector-ref ArrIndex 1) 19))
          (begin
            (vector-set! ArrIndex 1 0)
            (vector-set! ArrIndex 0 0))
          (if (equal? (vector-ref ArrIndex 1) 19) ;Increment vector position x if position y equals 19.
              (begin
                (vector-set! ArrIndex 0 (+ (vector-ref ArrIndex 0) 1))
                (vector-set! ArrIndex 1 0))
              (begin ;Else, increment vector position y by 1
                (vector-set! ArrIndex 1 (+ (vector-ref ArrIndex 1) 1))))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;PRINT THE MAP - Iterates through the arrayMap, calls printmap, ;;
    ;;                and increments xpos/ypos by 40px(tile size)    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (IterateAndPrint xpos ypos)
      (begin
        (printmap (array-ref arrayMap ArrIndex) xpos ypos)
        (incArrIndex)
        (if (and (equal? xpos 760) (equal? ypos 760)) (display "Done Generating \n") ;Hit edge of array. Stop printing.
          (if (equal? xpos 760) ;Else, Increment ypos by 40 if xpos is equal to 360; set xpos to 0.
            (IterateAndPrint 0 (+ ypos 40))
            (IterateAndPrint (+ xpos 40) ypos))))) ;Else, increment xpos by 40.
    ;;;;;;;;;;
    ;;DRIVER;;
    ;;;;;;;;;;
    (IterateAndPrint 0 0)))
;;;;;;;;;;;;;;;;;;;
;;END DISPLAY MAP;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RANDOMIZE THE ARRAY MAP - Calls all the randomization procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (randomizeArray)
    ;;GENERATE THE RANDOM PATH
    (genPath)
    ;;GENERATE WATER/LAVA
    ;;GENERATE OTHER THINGS
)
;;;;;;;;;;;;;;;;;;;;;
;;END RANDOMIZE MAP;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GENERATE PATH ALGORITHM: FOR 'path TAGGED TILES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (genPath)
  (begin
    ;;Define variables
    (define pathVector (vector 0 0)) ;Keeps track of current position in the map array
    (define nextx 0) ;Used to hold next step x value
    (define nexty 0) ;Used to hold next step y value
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; RANDOM WALK PROCEDURE - Randomly chooses a direction by setting nextx to a  ;;
    ;;                         random number from -1 to 1. -1=Down 0=Right 1=Up.   ;;
    ;;                         If nextx==0, set nexty to 1 in order to move right. ;;
    ;;                         Otherwise nexty is set to 0 to move up or down.     ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (chooseNext)
      (begin
        (set! nextx (- (random 3) 1)) ;Random number from -1 to 1.
        (if(equal? nextx 0) ;If next==0 set nexty to 1, else set nexty to 0.
           (set! nexty 1)(set! nexty 0))
          (if(and(> (+ nextx (vector-ref pathVector 0)) -1) ;Checks if the next step is in bounds
               (<(+ nextx (vector-ref pathVector 0)) 20))
            (if(eq?(array-ref arrayMap (vector (+ nextx (vector-ref pathVector 0)) ;Checks if the next step is empty(contains terrain)
                                               (+ nexty (vector-ref pathVector 1)))) (getTexture 'grass))
              (begin ;If the next step is terrain, Update the current vector position. Set that position in the map array
                     ;to the new path tile. Recrusively call this procedure again if the current position isn't against the right wall. 
                  (vector-set! pathVector 0 (+ nextx (vector-ref pathVector 0)))
                  (vector-set! pathVector 1 (+ nexty (vector-ref pathVector 1)))
                  (array-set! arrayMap pathVector (getTexture 'path))
                  (if(not(equal? (vector-ref pathVector 1) 19))
                     (chooseNext)(display "Generate path done\n")))
           ;If not empty or out of bounds, choose again by recursively calling this procedure.
           (chooseNext))
            (chooseNext))))
    ;;;;;;;;;;;;;;
    ;;  DRIVER  ;;
    ;;;;;;;;;;;;;;
    ;Picks a random point on the left edge of the map and places the first path tile.
    (vector-set! pathVector 0 (random 20))
    (vector-set! pathVector 1 0)
    (array-set! arrayMap pathVector (getTexture 'path))
    ;Generate the rest of the path. chooseNext will recursively call
    ;itself until it reaches the right edge of the map.
    (chooseNext)))
;;;;;;;;;;;;;;;;;;;;;
;;END GENERATE PATH;;
;;;;;;;;;;;;;;;;;;;;;

;;still in progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GENERATE LIQUID ALGORITHM: FOR 'liquid TAGGED TILES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (genLiquid waterVector)
    (begin
      (define limit 10)
      ;Used for next step
      (define nextx 0)
      (define nexty 0)
      ;Pick a random next spot next to the previous
      (define (chooseNext)
        (set! nextx (- (random 3) 1))
        (set! nexty (random 1))
        (if (and (equal? nextx 0) (equal? nexty 0))
            (set! nexty -1)
            (if (and (equal? nextx 0) (equal? nexty 1))
                (set! nexty -1) null))
        ;Check if the new postition is in bounds. Else, choose again.
        (if(and(> (+ nextx (vector-ref waterVector 0)) -1)
               (< (+ nextx (vector-ref waterVector 0)) 20)
               (> (+ nexty (vector-ref waterVector 1)) -1)
               (< (+ nexty (vector-ref waterVector 1)) 20))
           ;Check that the new position is empty(contains grass)
           (if(eq?(array-ref arrayMap (vector (+ nextx (vector-ref waterVector 0))
                                              (+ nexty (vector-ref waterVector 1)))) (getTexture 'grass))
              ;If empty, set pathVector to the new values.
              (begin
                (vector-set! waterVector 0 (+ nextx (vector-ref waterVector 0)))
                (vector-set! waterVector 1 (+ nexty (vector-ref waterVector 1))))
              ;If not empty, choose again.
              (chooseNext))
           (chooseNext)))
    ;Place the new tile in the arrayMap
    (define (placeTile)
      (begin
        (array-set! arrayMap waterVector (getTexture 'water))
        (set! limit (- limit 1))))
    ;Driver
    (if(> limit 0)
       (begin
         (chooseNext)
         (placeTile)
         (genLiquid waterVector))
       (display "Generate water done\n"))))
;;;;;;;;;;;;;;;;;;;;;;;
;;END GENERATE LIQUID;;
;;;;;;;;;;;;;;;;;;;;;;;

(send mainWin show #t)
