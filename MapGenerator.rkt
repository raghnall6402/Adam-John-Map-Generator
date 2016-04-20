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
            (makeTerrain #t (make-object bitmap%  "path.png")))
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
                  (min-width 400)
                  (min-height 400)))

;;Generate Button
(new button% [parent mainWin]
             [label "Generate"]
             ;;Button click calls GenerateMap
             [callback (lambda (button event)
                         (begin
                           (Reset)
                           (randomizeArray)
                           (GenerateMap)))])

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

;;Procedure for displaying the tiles
(define (printmap theTile xpos ypos)
    (send (send theMap get-dc) draw-bitmap theTile xpos ypos))

;;10x10 array for tiles(may be a dynamic array in the future)
(define arrayMap null)

(define (Reset)
    (set! arrayMap
       (mutable-array #[ #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)]
                         #[(getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass) (getTexture 'grass)] ])))

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

;; Randomly fill array
(define (randomizeArray)
  (begin
    (define x (random 10))
    (array-set! arrayMap (vector x 0) (getTexture 'path))
    (genPath (vector x 0))))

;;Generate path
(define (genPath pathVector)
  (begin
    ;;Used for next step
    (define nextx 0)
    (define nexty 0)
    ;;Determine next position and change pathVector value
    (define (chooseNext)
      (begin
        (set! nextx (- (random 3) 1))
        (if(equal? nextx 0)
           (set! nexty 1)(set! nexty 0))
        ;;If the new postition is in bounds. Else, choose again.
        (if(and(> (+ nextx (vector-ref pathVector 0)) -1)
               (<(+ nextx (vector-ref pathVector 0)) 10))
           ;;Check that the new position is empty(contains grass)
           (if(eq?(array-ref arrayMap (vector (+ nextx (vector-ref pathVector 0))
                                              (+ nexty (vector-ref pathVector 1)))) (getTexture 'grass))
              ;;If empty, set pathVector to the new values.
              (begin
                  (vector-set! pathVector 0 (+ nextx (vector-ref pathVector 0)))
                  (vector-set! pathVector 1 (+ nexty (vector-ref pathVector 1))))
           ;;If not empty, choose again.
           (chooseNext))
            (chooseNext))))
    ;;Place the new tile in the arrayMap
    (define (placeTile)
      (array-set! arrayMap pathVector (getTexture 'path)))
   ;;Driver
    (if(not(equal? (vector-ref pathVector 1) 9))
       (begin
         (chooseNext)
         (placeTile)
         (genPath pathVector))
       (display "Generate path done\n"))))

(send mainWin show #t)
