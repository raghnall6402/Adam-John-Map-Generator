# Random Map Generator

##Authors
Adam Melle
John Perkins

##Overview
Our project is a map generator that generates random game tilemaps. The program allows the user to adjust the chance to generate certain elements of the map. The generate button can then be clicked to generate a unique map each time. The maps generated will always have a clear path from the left side to the right, without anything obstructing the path. 

##Screenshot
Here's an example of a randomly generated map.
![screenshot example map](examplemap-new.png)

##Concepts Demonstrated
Adam: I used a lot of recursion in the randomization procedures for the path/water/lava. The randomization procedures also used local state variables that were modified within the procedure during that instance of the procedure call. They were used and modified to keep track of the number of water/lava tiles that could be generated during the recursive procedure. Mutable data was also used a lot in the procedures and for the array. Vectors were constantly being set to different values in order to move around and modify the array. 

##External Technology and Libraries
racket/gui - Used for the UI and for printing the map to the window. <br>
math/array - Used for the array map.<br>
Everything else was already in the racket language. <br>
Free and modified graphics were used for the map. <br>

##Favorite Scheme Expressions
####Adam
My favorite procedure, which is the one I'm most proud of, is the genPath procedure. The reason it's my favorite is because it has two functionalities built into the same procedure. Initially, it is used to generate a path by choosing a random point on the left side of the map and then perform a random walk until it reaches the the right side of the map. If the path complexity slider is greater than 0 after the first call to the procedure, it has a chance to recursively call itself, but with the subpath parameter as #t. This causes the procedure to act differently. This time, instead of choosing a starting point on the left side, it targets a random point on the already existing path and then branches off of it until it either hits its tilegen limit, or it reaches the right edge of the map.
```scheme
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
```

#How to Download and Run
The program should run fine in racket as is. Package updates for gui and array should be the only possible conflicts.
