#lang racket/gui
(require 2htdp/image)
(require math/array)

;;HASHTABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; END OF HASHTABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; WINDOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Map class containing overridden canvas on-paint class
;;The main window (mainWin)
(define mainWin (instantiate frame%("Map Generator")))

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

;;Path complexity slider
(define pathComplexity
  (new slider% (label "Path complexity")
               (parent mainWin)
               (min-value 0)
               (max-value 100)
               (init-value 50)))

;;Water intensity slider
(define waterIntensity
  (new slider% (label "Water intensity")
               (parent mainWin)
               (min-value 0)
               (max-value 100)
               (init-value 50)))

;;Lava intensity slider
(define lavaIntensity
  (new slider% (label "Lava intensity  ")
               (parent mainWin)
               (min-value 0)
               (max-value 100)
               (init-value 50)))

;;Generate Button
(new button% [parent mainWin]
             [label "Generate"]
             ;;Button click resets the map, randomizes it, then displays it.
             [callback (lambda (button event)
                         (begin
                           (Reset)
                           (randomizeArray)
                           (DisplayMap)))])

;; CHECK BOXES FOR FILTERING TERRAIN
(define (checkBoxes lst)
  (if (equal? (cdr lst) '())
      (new check-box%
           (label (symbol->string (car lst)))
           (parent mainWin)
           ;(callback (boxesChecked))
           (value #f))
      (begin (new check-box%
                   (label (symbol->string (car lst)))
                   (parent mainWin)
                   ;(callback (boxesChecked)) ;(getTexture (car lst)))
                   (value #f))
             (checkBoxes (cdr lst))
             )))

(define (boxesChecked)
  (display "boxes checked\n"))

;; END OF WINDOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; TERRAIN RANDOMIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define terItems 0)

(define terrainList (list))

(define (randomTerrain)
  (let ((ranTer (random terItems)))
    (define (randomTerrain-h current list)
      (if (= current ranTer) (car list)
          (randomTerrain-h (+ 1 current) (cdr list))))
  (randomTerrain-h 0 terrainList)))

(define (addToTerrainList item)
  (begin (set! terrainList (append terrainList item))
         (set! terItems (+ 1 terItems))))
;; END OF TERRAIN RANDOMIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TERRAIN OBJECTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; also puts it into the terrain list for randomization
(define (newTerrain tag passable image)
  (begin (putTerrain tag (makeTerrain passable (make-object bitmap% image)))
         (addToTerrainList (list tag))))

; liquid constructor does not put it into the list to be selected randomly
(define (newLiquid tag passable image)
  (putTerrain tag (makeTerrain passable (make-object bitmap% image))))

;;DEFAULT TERRAIN
(putTerrain 'default (makeTerrain #f (make-object bitmap% "blank.bmp")))
(putTerrain 'path (makeTerrain #t (make-object bitmap% "path.png")))

;;NORMAL OBJECTS
(newTerrain 'grass1 #t "grass1.png")
(newTerrain 'grass2 #t "grass2.png")
(newTerrain 'grass3 #t "grass3.png")
(newTerrain 'grass4 #t "grass4.png")
(newTerrain 'ice #t "ice.png")

;(checkBoxes terrainList)
; terrains other than the grasses screw up the time to generate the map
;can add a new terrain by just a simple
; (newTerrain 'snow #t "snow.png")
; and have it be generated into the map


;;LIQUID OBJECTS ARE SPECIAL FOR NOW
(newLiquid 'lava #f "lava.png")
(newLiquid 'water #f "water.png")

;; EMD OF TERRAIN OBJECTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (normalTerrain? item)
  (define (normal-terrain-helper lst)
    (cond ((equal? lst '()) #f)
          ((equal? item (getTexture (car lst))) #t)
          (else (normal-terrain-helper (cdr lst)))))
  (normal-terrain-helper terrainList))

;;Procedure for printing
(define (printmap theTile xpos ypos)
    (send (send theMap get-dc) draw-bitmap theTile xpos ypos))

;;20x20 array for tiles(may be a dynamic array in the future)
(define arrayMap null)

;; NOT OPTIMIZED?
(define (Reset)
    (set! arrayMap
       (mutable-array #[ #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]
                         #[(getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain)) (getTexture (randomTerrain))]])))

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
  (begin
    ;;GENERATE THE RANDOM PATH
    (genPath (send pathComplexity get-value) #f)
    ;;GENERATE WATER
    (genLiquid 'water (send waterIntensity get-value))
    ;;GENERATE LAVA
    (genLiquid 'lava (send lavaIntensity get-value))
    ;;GENERATE OTHER THINGS
))
;;;;;;;;;;;;;;;;;;;;;
;;END RANDOMIZE MAP;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GENERATE PATH ALGORITHM: FOR 'path TAGGED TILES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (genPath complexity subpath)
  (begin
    ;;Define variables
    (define pathVector (vector 0 0)) ;Keeps track of current position in the map array
    (define nextx 0) ;Used to hold next step x value
    (define nexty 0) ;Used to hold next step y value
    (define sublength 100)
    (define attempts 200)
    ;;;;;;;;;;;;;;;;;;;;;;
    ;;Branch from path  ;;
    ;;;;;;;;;;;;;;;;;;;;;;
    (define (branchRandom)
      (if(> attempts 0)
         (begin
           (set! attempts (- attempts 1))
           (vector-set! pathVector 0 (random 20))
           (vector-set! pathVector 1 (random 20))
           (if(eq?(array-ref arrayMap (vector (vector-ref pathVector 0) ;Checks if random point is a path tile
                                      (vector-ref pathVector 1))) (getTexture 'path))
              (chooseNext)(branchRandom)))(display "Used every attempt")))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; RANDOM WALK PROCEDURE - Randomly chooses a direction by setting nextx to a  ;;
    ;;                         random number from -1 to 1. -1=Down 0=Right 1=Up.   ;;
    ;;                         If nextx==0, set nexty to 1 in order to move right. ;;
    ;;                         Otherwise nexty is set to 0 to move up or down.     ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (chooseNext)
      (if(or(and(equal? subpath #t)(> sublength 0))(equal? subpath #f))
        (begin
          (set! sublength (- sublength 1))
          (set! nextx (- (random 3) 1)) ;Random number from -1 to 1.
          (if(equal? nextx 0) ;If next==0 set nexty to 1, else set nexty to 0.
             (set! nexty 1)(set! nexty 0))
            (if(and(> (+ nextx (vector-ref pathVector 0)) -1) ;Checks if the next step is in bounds
                   (< (+ nextx (vector-ref pathVector 0)) 20)
                   (> (+ nexty (vector-ref pathVector 1)) -1)
                   (< (+ nexty (vector-ref pathVector 1)) 20))
              (if(or(normalTerrain? (array-ref arrayMap (vector (+ nextx (vector-ref pathVector 0)) ;Checks if the next step is empty(contains terrain)
                                                    (+ nexty (vector-ref pathVector 1)))))
                    (and(eq?(array-ref arrayMap (vector (+ nextx (vector-ref pathVector 0)) ;Checks if the next step is a path(this option is for subpaths)
                                                        (+ nexty (vector-ref pathVector 1)))) (getTexture 'path))
                      (equal? subpath #t))) 
                (begin ;If the next step is terrain, Update the current vector position. Set that position in the map array
                       ;to the new path tile. Recrusively call this procedure again if the current position isn't against the right wall. 
                    (vector-set! pathVector 0 (+ nextx (vector-ref pathVector 0)))
                    (vector-set! pathVector 1 (+ nexty (vector-ref pathVector 1)))
                    (array-set! arrayMap pathVector (getTexture 'path))

                    (if(and(not(equal? (vector-ref pathVector 1) 19)))
                      (chooseNext)
                      (display "Generate path done\n")))
             ;If not empty or out of bounds, choose again by recursively calling this procedure.
             (chooseNext))
              (chooseNext)))null))
    ;;;;;;;;;;;;;;
    ;;  DRIVER  ;;
    ;;;;;;;;;;;;;;
    ;;If the call to this procedure is meant to generate a subpath
    (if(and (< (random 120) complexity) (equal? subpath #t))
      (begin
        (branchRandom)
        (genPath complexity #t))null)
    ;Picks a random point on the left edge of the map and places the first path tile.
    (if(equal? subpath #f)
      (begin
        (vector-set! pathVector 0 (random 20))
        (vector-set! pathVector 1 0)
        (array-set! arrayMap pathVector (getTexture 'path))
        ;Generate the rest of the path. chooseNext will recursively call
        ;itself until it reaches the right edge of the map.
        (chooseNext))null)
    ;;If the complexity is > 0 and branch is #f, set branch to #t and recrusively call genpath
    (if(and(> complexity 0)(equal? subpath #f))
       (genPath complexity #t)(display "no subpaths"))))

;;;;;;;;;;;;;;;;;;;;;
;;END GENERATE PATH;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GENERATE LIQUID ALGORITHM: FOR 'liquid TAGGED TILES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (genLiquid type intensity)
    (begin
      ;;Define variables
      (define waterVector (vector 0 0)) ;Keeps track of current position in the map array
      (define liquidLimit ;Number of water tiles to spawn
        (if (< intensity 15)
            15 intensity))
      (define nextx 0) ;Used for next step
      (define nexty 0) ;Used for next step
      (define tilechance 3) ;2/3 chance
      (define turnchance 0) ;0% chance to turn initially
      (define direction 0) ;0-right 1-down 2-left 3-up
      (define attempts 15) ;number of times random start can try to find a random point. prevents endless loop if the map is full.
      ;;;;;;;;;;;;;;;;;;;;;;
      ;;Random start point;;
      ;;;;;;;;;;;;;;;;;;;;;;
      (define (startRandom)
        (if (> attempts 0)
            (begin
              (set! attempts (- attempts 1))
              (set! nextx (random 20))
              (set! nexty (random 20))
              (if (inBounds?)
                  (if (tileEmpty?)
                      (begin
                        (placeTile)
                        (Turn))
                      (startRandom))
                  (startRandom)))(display "Ran out of attempts")))
      ;;;;;;;;;;;;;;;;;;;;;
      ;;Changes direction;;
      ;;;;;;;;;;;;;;;;;;;;;
      (define (Turn)
        (begin
          (set! turnchance (+ turnchance 28))
          (if(< (random 100) turnchance)
             (begin
               (set! turnchance 0)
               (if (equal? direction 3)
                   (set! direction 0)
                   (set! direction (+ direction 1))))
             null)))
      ;;;;;;;;;;;;;;
      ;;Force turn;;
      ;;;;;;;;;;;;;;
      (define (forceTurn)
        (begin
          (set! liquidLimit (- liquidLimit 1))
          (set! turnchance 0)
          (if (equal? direction 3)
            (set! direction 0)
            (set! direction (+ direction 1)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;Checks if position is in bounds;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (inBounds?)
        (if (and (> (+ nextx (vector-ref waterVector 0)) -1)
                 (< (+ nextx (vector-ref waterVector 0)) 20)
                 (> (+ nexty (vector-ref waterVector 1)) -1)
                 (< (+ nexty (vector-ref waterVector 1)) 20))#t #f))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;Checks if position is empty;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (tileEmpty?)
        (normalTerrain? (array-ref arrayMap (vector (+ nextx (vector-ref waterVector 0))
                                              (+ nexty (vector-ref waterVector 1))))))
          
      ;;;;;;;;;;;;;;
      ;;Place tile;;
      ;;;;;;;;;;;;;;
      (define (placeTile)
        (begin
          (set! liquidLimit (- liquidLimit 1))
          (vector-set! waterVector 0 (+ nextx (vector-ref waterVector 0)))
          (vector-set! waterVector 1 (+ nexty (vector-ref waterVector 1)))
          (array-set! arrayMap waterVector (getTexture type))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;Take a step in a direction;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (Step)
        (begin
          (cond
            [(equal? direction 0)(begin (set! nextx 0)(set! nexty 1))] ;move right
            [(equal? direction 1)(begin (set! nextx 1)(set! nexty 0))] ;move down
            [(equal? direction 2)(begin (set! nextx 0)(set! nexty -1))] ;move left
            [(equal? direction 3)(begin (set! nextx -1)(set! nexty 0))]) ;move up
          (if (inBounds?)
            (if (tileEmpty?)
              (begin
                (placeTile)
                (Turn))
              (forceTurn))
              (forceTurn))
          (if (> liquidLimit 0)
            (Step)
            (display "Generate Liquid Done \n"))))
      ;;;;;;;;;;;;;;
      ;;  DRIVER  ;;
      ;;;;;;;;;;;;;;
      (if (> intensity 0)
        (begin
          (startRandom)
          (Step))null)
      (if(< (random 115) intensity)
         (genLiquid type intensity)null)))
;;;;;;;;;;;;;;;;;;;;;;;
;;END GENERATE LIQUID;;
;;;;;;;;;;;;;;;;;;;;;;;

(send mainWin show #t)
