;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Frogger) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Frogger Project
;; Thedita Pedersen
;; 6/4/2020

;; Exercise 1:
;; Review code from Hw 6 and rewrite according to mark ups/ fix problems
;; Clean it up with list abstractions, local, lambda
;; Upgrade Frogger to include:
;; - five rows of river (check)
;;    - player must collide with a turtle or a plank at all times (check)
;;    - planks move right and turtles move left (check)
;;    - if player lands in river, game over (check
;;    - player loses if they leave the bounds of the screen (check)
;;    - planks and turtles have the same length and speed (check)
;;    - player can move up, down, left and right (check)
;;         - when player moves the frog turns to match that direction
;;         - my frog is a circle and doesn't have a face to turn with?

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define BG-WIDTH 700)
(define ROW-NUMS 13)
(define BG-HEIGHT (* 50 ROW-NUMS))
(define ROW (/ BG-HEIGHT ROW-NUMS))
(define SAFE-ROW (rectangle BG-WIDTH ROW "solid" "DarkGreen"))
(define TRAFFIC-ROW (rectangle BG-WIDTH ROW "solid" "black"))
(define RIVER-ROW (rectangle BG-WIDTH ROW "solid" "RoyalBlue"))
(define EMPTY (empty-scene BG-WIDTH BG-HEIGHT))

(define BG (above
            SAFE-ROW
            RIVER-ROW
            RIVER-ROW
            RIVER-ROW
            RIVER-ROW
            RIVER-ROW
            SAFE-ROW
            TRAFFIC-ROW
            TRAFFIC-ROW
            TRAFFIC-ROW
            TRAFFIC-ROW
            TRAFFIC-ROW
            SAFE-ROW))

;; Y positions of the middle of each row
(define ROW-1 (- BG-HEIGHT (/ ROW 2)))
(define ROW-2 (- BG-HEIGHT ROW (/ ROW 2)))
(define ROW-3 (- BG-HEIGHT (* 2 ROW) (/ ROW 2)))
(define ROW-4 (- BG-HEIGHT (* 3 ROW) (/ ROW 2)))
(define ROW-5 (- BG-HEIGHT (* 4 ROW) (/ ROW 2)))
(define ROW-6 (- BG-HEIGHT (* 5 ROW) (/ ROW 2)))
(define ROW-7 (- BG-HEIGHT (* 6 ROW) (/ ROW 2)))
(define ROW-8 (- BG-HEIGHT (* 7 ROW) (/ ROW 2)))
(define ROW-9 (- BG-HEIGHT (* 8 ROW) (/ ROW 2)))
(define ROW-10 (- BG-HEIGHT (* 9 ROW) (/ ROW 2)))
(define ROW-11 (- BG-HEIGHT (* 10 ROW) (/ ROW 2)))
(define ROW-12 (- BG-HEIGHT (* 11 ROW) (/ ROW 2)))
(define ROW-13 (- BG-HEIGHT (* 12 ROW) (/ ROW 2)))

(define NEAR-BOTTOM (* 0.95 BG-HEIGHT))
(define NEAR-TOP (* .05 BG-HEIGHT))

(define DOT-SIZE 10)
(define SCALE (/ DOT-SIZE 2))
(define PLAYER-IMG (circle DOT-SIZE "solid" "YellowGreen")) ;;DEVELOP FROG IMAGE LATER
(define CAR-IMG (square DOT-SIZE "solid" "Crimson"))
(define PLANK-WIDTH (* 3 DOT-SIZE))
(define PLANK-HEIGHT (* 2 DOT-SIZE))
(define TURTLE-WIDTH PLANK-WIDTH)
(define TURTLE-HEIGHT PLANK-HEIGHT)
(define TURTLE-IMG (ellipse TURTLE-WIDTH TURTLE-HEIGHT "solid" "SeaGreen"))
(define PLANK-IMG (rectangle PLANK-WIDTH PLANK-HEIGHT  "solid" "Sienna"))
(define LOST-IMG (overlay (text "You lost! Try again!" 20 "white") BG))
(define WON-IMG (overlay (text "You won!! :D" 20 "white") BG))

(define PLAYER-SPEED 5)
(define VEHICLE-SPEED 3)
(define RIVER-SPEED 3)

(define PLAYER-MIN DOT-SIZE)
(define PLAYER-MAX-X (- BG-WIDTH PLAYER-MIN))
(define PLAYER-MAX-Y (- BG-HEIGHT PLAYER-MIN))

;;;;;;;;;;;;;;;;;;;;;;; DATA DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Direction is one of: "left" | "right"
;; the two directions traffic can move in
(define DIR-R "right")
(define DIR-L "left")

;; direction-templ: Direction -> ??
#;(define (direction-templ d)
    ... (cond [(string=? d "right") ...]
              [(string=? d "left") ...]))

(define-struct player (x y))
;; A Player is a (make-player Number Number)
;; represents the x and y position of a player
(define PLAYER-INITIAL (make-player (/ BG-WIDTH 2) ROW-1))
(define PLAYER-1 (make-player 200 300))

;; player-templ: Player -> ??
#;(define (player-templ p)
    ... (player-x p)
    ... (player-y p)...)

(define-struct vehicle (x y dir))
;; A Vehicle is a (make-vehicle Number Number Direction)
;; represents the x and y position of a vehicle and
;; the direction it's travelling in
(define CAR2-1 (make-vehicle 140 ROW-2 "left")) ;; 1st car in the 2nd row 
(define CAR2-2 (make-vehicle 280 ROW-2 "left")) ;; 2nd car in the 2nd row
(define CAR2-3 (make-vehicle 420 ROW-2 "left")) ;; 3rd car in the 2nd row
(define CAR2-4 (make-vehicle 560 ROW-2 "left")) ;; 4th car in the 2nd row

(define CAR3-1 (make-vehicle 140 ROW-3 "right")) ;; 1st car in the 3rd row
(define CAR3-2 (make-vehicle 280 ROW-3 "right")) ;; 2nd car in the 3rd row
(define CAR3-3 (make-vehicle 420 ROW-3 "right")) ;; 3rd car in the 3rd row
(define CAR3-4 (make-vehicle 560 ROW-3 "right"));; 4th car in the 3rd row

(define CAR4-1 (make-vehicle 140 ROW-4 "left")) ;; and so on
(define CAR4-2 (make-vehicle 280 ROW-4 "left")) 
(define CAR4-3 (make-vehicle 420 ROW-4 "left")) 
(define CAR4-4 (make-vehicle 560 ROW-4 "left")) 

(define CAR5-1 (make-vehicle 140 ROW-5 "right")) 
(define CAR5-2 (make-vehicle 280 ROW-5 "right")) 
(define CAR5-3 (make-vehicle 420 ROW-5 "right")) 
(define CAR5-4 (make-vehicle 560 ROW-5 "right"))

(define CAR6-1 (make-vehicle 140 ROW-6 "left")) 
(define CAR6-2 (make-vehicle 280 ROW-6 "left")) 
(define CAR6-3 (make-vehicle 420 ROW-6 "left")) 
(define CAR6-4 (make-vehicle 560 ROW-6 "left")) 


;; vehicle-templ: Vehicle -> ??
#;(define (vehicle-templ v)
    ... (vehicle-x v)
    ... (vehicle-y v)
    ... (vehicle-dir v))

;; rename this later if time
;; A Set of Vehicles (VSet) is one of:
;; - empty
;; - (cons Vehicle VSet)
;; represents either a row with no vehicles or
;; a row with at least 1 vehicle
(define VSET-1 '())
(define EX-VSET (list CAR2-1))
(define VSET-2 (list CAR2-4 CAR2-3 CAR2-2 CAR2-1))
(define VSET-3 (append (list CAR3-4 CAR3-3 CAR3-2 CAR3-1) VSET-2))
(define VSET-4 (append (list CAR4-4 CAR4-3 CAR4-2 CAR4-1) VSET-3))
(define VSET-5 (append (list CAR5-4 CAR5-3 CAR5-2 CAR5-1) VSET-4))
(define VSET-6 (append (list CAR5-4 CAR5-3 CAR5-2 CAR5-1) VSET-5))
(define VSET-INITIAL (append (list CAR6-4 CAR6-3 CAR6-2 CAR6-1) VSET-6))

;; vset-templ: VSet -> ??
#;(define (vset-templ vset)
    (cond [(empty? vset) ...]
          [(cons? vset) ... (vehicle-templ (first vset))
                        ... (vset-templ (rest vset))]))

(define-struct turtle (x y dir))
;; A Turtle is a (make-turtle Number Number Direction)
;; represents the x and y position of a turtle (turtles always move left)
(define TURT9-1 (make-turtle 140 ROW-9 "left"))
(define TURT9-2 (make-turtle 280 ROW-9 "left"))
(define TURT9-3 (make-turtle 420 ROW-9 "left"))
(define TURT9-4 (make-turtle 560 ROW-9 "left"))

(define TURT11-1 (make-turtle 140 ROW-11 "left"))
(define TURT11-2 (make-turtle 280 ROW-11 "left"))
(define TURT11-3 (make-turtle 420 ROW-11 "left"))
(define TURT11-4 (make-turtle 560 ROW-11 "left"))

;; turtle-templ: Turtle -> ??
#;(define (turtle-templ t)
    ... (turtle-x t)
    ... (turtle-y t)
    ... (turtle-dir t))

;; A ListofTurtle (LoT) is a [List-of Turtle]
;; represents the turtles in a game
(define LOT-0 '())
(define EX-LOT (list TURT9-1))
(define LOT-1 (list TURT9-1 TURT9-2 TURT9-3 TURT9-4))
(define LOT-2 (list TURT11-1 TURT11-2 TURT11-3 TURT11-4))
(define LOT-INITIAL (append LOT-1 LOT-2))

;; lot-templ: LoT -> ??
#;(define (lot-templ lot)
    (cond [(empty? lot) ...]
          [(cons? lot) ... (turtle-templ (first lot))
                       ... (lot-templ (rest lot))]))

(define-struct plank (x y dir))
;; A Plank is a (make-plank Number Number Direction)
;; represents the x and y position of a plank (planks always move right)
(define PLANK8-1 (make-plank 140 ROW-8 "right"))
(define PLANK8-2 (make-plank 280 ROW-8 "right"))
(define PLANK8-3 (make-plank 420 ROW-8 "right"))
(define PLANK8-4 (make-plank 560 ROW-8 "right"))

(define PLANK10-1 (make-plank 140 ROW-10 "right"))
(define PLANK10-2 (make-plank 280 ROW-10 "right"))
(define PLANK10-3 (make-plank 420 ROW-10 "right"))
(define PLANK10-4 (make-plank 560 ROW-10 "right"))

(define PLANK12-1 (make-plank 140 ROW-12 "rigth"))
(define PLANK12-2 (make-plank 280 ROW-12 "right"))
(define PLANK12-3 (make-plank 420 ROW-12 "right"))
(define PLANK12-4 (make-plank 560 ROW-12 "right"))

;; plank-templ: Plank -> ??
#;(define (plank-templ p)
    ... (plank-x p)
    ... (plank-y p)
    ... (plank-dir p))

;; A ListofPlank (LoP) is a [List-of Plank]
;; represents the planks in a game
(define LOP-0 '())
(define EX-LOP (list PLANK8-1))
(define LOP-1 (list PLANK8-1 PLANK8-2 PLANK8-3 PLANK8-4))
(define LOP-2 (list PLANK10-1 PLANK10-2 PLANK10-3 PLANK10-4))
(define LOP-3 (list PLANK12-1 PLANK12-2 PLANK12-3 PLANK12-4))
(define LOP-INITIAL (append LOP-1 LOP-2 LOP-3))

;; A Object is one of:
;; - Plank
;; - Turtle
;; - Vehicle
;; repr. the objects in the game
(define OBJECT-1 CAR2-1)
(define OBJECT-2 TURT9-1)
(define OBJECT-3 PLANK8-1)

;; object-templ: Object -> ?
#;(define (object-templ o)
    (cond [(vehicle? o) ...]
          [(turtle? o) ...]
          [(plank? o) ...]))

;; A ListOfObjects (LoO) isa [List-of Object]
;; repr. a set of objects
(define LOO-0 '())
(define EX-LOO (append EX-VSET EX-LOT EX-LOP))
(define LOO-INITIAL (append VSET-INITIAL LOT-INITIAL LOP-INITIAL))

;; loo-templ: LoO -> ??
#;(define (loo-templ loo)
    ... (cond [(empty? loo) ...]
              [(cons? loo) ... (object-templ (first loo))
                           ... (loo-templ (rest loo))]))
              
(define-struct world (player objects))
;; A World is a (make-world Player LoO)
;; repr. world state where
;; the Player is the position of the player and
;; the LoO are the objects moving in the world
(define WORLD-1 (make-world PLAYER-INITIAL LOO-0))
(define WORLD-2 (make-world PLAYER-1 EX-LOO))
(define WORLD-3 (make-world PLAYER-1 (append (list CAR3-2 PLANK10-2 TURT11-2) EX-LOO)))

;; world-templ: World -> ??
#;(define (world-templ w)
    ... (player-templ (world-player w))
    ... (loo-templ (world-objects w)))

;;;;;;;;;;;;;;;;;;; MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define INITIAL-WORLD (make-world PLAYER-INITIAL LOO-INITIAL))

;; play-frogger: World -> World
;; play the frogger game

(define (play-frogger initial-w)
  (big-bang initial-w
    [to-draw draw-game]
    [on-tick move-world]
    [on-key move-player]
    [stop-when game-over? draw-end-scene]))

;;;;;;;;;;;;;;;;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;
(define WORLD-2-IMG (place-image PLANK-IMG 140 ROW-8
                                 (place-image TURTLE-IMG 140 ROW-9
                                              (place-image CAR-IMG 140 ROW-2
                                                           (place-image PLAYER-IMG 200 300 BG)))))
;; draw-game: World -> Image
;; draw the player and all objects in world on the background
(check-expect (draw-game WORLD-1)
              (place-image PLAYER-IMG (/ BG-WIDTH 2) ROW-1 BG))
(check-expect (draw-game WORLD-2) WORLD-2-IMG)
(check-expect (draw-game WORLD-3)
              (place-image CAR-IMG 280 ROW-3
                           (place-image PLANK-IMG 280 ROW-10
                                        (place-image TURTLE-IMG 280 ROW-11 WORLD-2-IMG))))

(define (draw-game w)
  (local [(define PLAYER (world-player w))
          (define LOO (world-objects w))
          ;; draw-together: LoO -> Image
          ;; LOO-0 -> BG
          ;; LOO-1 -> (place-image PLANK-IMG 140 ROW-8
          ;               (place-image TURTLE-IMG 140 ROW-9
          ;                      (place-image CAR-IMG 140 ROW-2 BG)))
          (define (draw-together loo)
            (foldr draw BG loo))
          ;; draw: Object Image -> Image
          ;; CAR2-1 BG -> (place-image CAR-IMG 140 ROW-2 BG)
          ;; TURT9-1 BG -> (place-image TURTLE-IMG 140 ROW-9 BG)
          ;; PLANK8-1 BG -> (place-image PLANK-IMG 140 ROW-8 BG)
          (define (draw o1 img)
            (cond [(vehicle? o1) (draw-thing vehicle-x vehicle-y o1 CAR-IMG img)]
                  [(turtle? o1)  (draw-thing turtle-x turtle-y o1 TURTLE-IMG img)]
                  [(plank? o1) (draw-thing plank-x plank-y o1 PLANK-IMG img)]))
          ;; draw-thing: (X) [X -> Number] [X -> Number] X Image Image -> Image
          ;; vehicle-x vehicle-y CAR2-1 CAR-IMG BG -> (place-image CAR-IMG 140 ROW-2 BG)
          ;; player-x player-y PLAYER-INITIAL PLAYER-IMG BG ->
          ;; (place-image PLAYER-IMG (/ BG-WIDTH 2) ROW-1 BG)
          (define (draw-thing thing-x thing-y t img bg)
            (place-image img (thing-x t) (thing-y t) bg))
          ;; draw-player: Player Image -> Image
          ;; PLAYER-INITIAL -> (place-image PLAYER-IMG (/ BG-WIDTH 2) ROW-1 BG)
          ;; PLAYER-1 -> (place-image PLAYER-IMG 200 300 BG)
          (define (draw-player p bg)
            (draw-thing player-x player-y p PLAYER-IMG bg))]
    (draw-player PLAYER (draw-together LOO))))

;; move-world: World -> World
;; move the player if on river object and move objects in the world in their given direction
;; if object hits end of row, new object spawns at start of row
;(check-expect (move-world WORLD-1) WORLD-1)
(check-expect (move-world WORLD-2)
              (make-world PLAYER-1
                          (list (make-vehicle (- 140 VEHICLE-SPEED) ROW-2 "left")
                                (make-turtle (- 140 RIVER-SPEED) ROW-9 "left")
                                (make-plank (+ 140 RIVER-SPEED) ROW-8 "right"))))
(check-expect (move-world
               (make-world PLAYER-1
                           (list (make-vehicle (- SCALE) ROW-2 "left")
                                 (make-turtle (- SCALE) ROW-9 "left")
                                 PLANK8-1)))
              (make-world PLAYER-1
                          (list (make-vehicle (+ BG-WIDTH SCALE) ROW-2 "left")
                                (make-turtle (+ BG-WIDTH SCALE) ROW-9 "left")
                                (make-plank (+ 140 RIVER-SPEED) ROW-8 "right"))))
(check-expect (move-world (make-world (make-player 140 ROW-8) (list CAR3-1 TURT9-1 PLANK8-1)))
              (make-world (make-player (+ 140 RIVER-SPEED) ROW-8)
                          (list (make-vehicle (+ 140 VEHICLE-SPEED) ROW-3 "right")
                                (make-turtle (- 140 RIVER-SPEED) ROW-9 "left")
                                (make-plank (+ 140 RIVER-SPEED) ROW-8 "right"))))
(check-expect (move-world
               (make-world (make-player 140 ROW-9)
                           (list (make-vehicle (+ BG-WIDTH SCALE) ROW-3 "right")
                                 TURT9-1
                                 (make-plank (+ BG-WIDTH SCALE) ROW-8 "right"))))
              (make-world (make-player (- 140 RIVER-SPEED) ROW-9)
                          (list (make-vehicle (- SCALE) ROW-3 "right")
                                (make-turtle (- 140 RIVER-SPEED) ROW-9 "left")
                                (make-plank (- SCALE) ROW-8 "right"))))

(define (move-world w)
  (local [(define LOO (world-objects w))
          (define PLANKS (filter plank? LOO))
          (define TURTLES (filter turtle? LOO))
          ;; move-player : Player LoO -> Player
          ;; move the player with the current iff on plank or turtle
          ;; (make-player 140 ROW-9) (list EX-LOT) -> (make-player (- 140 RIVER-SPEED) ROW-9)
          ;; (make-player 140 ROW-8) (list EX-LOP) -> (make-player (+ 140 RIVER-SPEED) ROW-8)
          ;; PLAYER-INITIAL LOO-0 -> PLAYER-INITIAL
          (define (move-player p loo)
            (cond [(on-plank? p loo) (make-player (+ (player-x p) RIVER-SPEED) (player-y p))]
                  [(on-turtle? p loo) (make-player (- (player-x p) RIVER-SPEED) (player-y p))]
                  [else p]))
          ;; on-plank? : Player LoO -> Boolean
          ;; Is the player on a plank?
          ;; (make-player 140 ROW-8) (append EX-LOT EX-LOP) -> #t
          ;; PLAYER-INITIAL (append EX-LOT EX-LOP) -> #f
          ;; PLAYER-INITIAL EX-LOT -> #f
          (define (on-plank? p loo)
            (ormap (λ (plank) (within-plank? p plank)) PLANKS))
          ;; on-turtle? : Player LoO -> Boolean
          ;; Is the player on a turtle?
          ;; (make-player 140 ROW-9) (append EX-LOT EX-LOP) -> #t
          ;; PLAYER-INITIAL (append EX-LOT EX-LOP) -> #f
          ;; PLAYER-INITIAL  EX-LOT -> #f
          (define (on-turtle? p loo)
            (ormap (λ (turtle) (within-turtle? p turtle)) TURTLES)) 
          ;; move-objects : LoO -> LoO
          ;; move objects in the world in their given direction, spawn if leave game window
          ;; EX-LOO -> (list (make-vehicle (- 140 VEHICLE-SPEED) ROW-2 "left")
          ;(make-turtle (- 140 RIVER-SPEED) ROW-9 "left")
          ;(make-plank (+ 140 RIVER-SPEED) ROW-8 "right"))))
          ;; LOO-0 -> '()
          (define (move-objects loo)
            (map move loo))
          ;; move : Object -> Object
          ;; move each object in given direction, spawn at start of row if leaves game window
          ;; CAR2-1 -> (make-vehicle (- 140 VEHICLE-SPEED) ROW-2 "left")
          ;; TURT9-1 -> (make-turtle (- 140 RIVER-SPEED) ROW-9 "left")
          ;; PLANK8-1 -> (make-plank (+ 140 RIVER-SPEED) ROW-8 "right")
          (define (move o)
            (cond [(vehicle? o) (move-v o)]
                  [(turtle? o) (move-left turtle-x turtle-y make-turtle o RIVER-SPEED)]
                  [(plank? o) (move-right plank-x plank-y make-plank o RIVER-SPEED)]))
          ;; move-v : Vehicle -> Vehicle
          ;; move vehicle in given direction, spawn at start of row if leaves game window
          ;; CAR3-1 -> (make-vehicle (+ 140 VEHICLE-SPEED) ROW-3 "right")
          ;; CAR2-1 -> (make-vehicle (- 140 VEHICLE-SPEED) ROW-2 "left")
          ;; (make-vehicle (- SCALE) ROW-2 "left") -> (make-vehicle (+ BG-WIDTH SCALE) ROW-2 "left")
          (define (move-v v)
            (if (string=? (vehicle-dir v) "left")
                (move-left vehicle-x vehicle-y make-vehicle v VEHICLE-SPEED)
                (move-right vehicle-x vehicle-y make-vehicle v VEHICLE-SPEED)))
          ;; move-right: (X) [X -> Number] [X -> Number](make-X Number Number String) X Number -> X
          ;; moves an item in the game to the right
          ;;  vehicle-x vehicle-y make-vehicle CAR3-1 VEHICLE-SPEED ->
          ;;  (make-vehicle (+ 140 VEHICLE-SPEED) ROW-3 "right"))
          ;; vehicle-x vehicle-y make-vehicle (make-vehicle (+ BG-WIDTH SCALE) ROW-3 "right")
          ;; VEHICLE-SPEED) -> (make-vehicle (- SCALE) ROW-3 "right")
          (define (move-right thing-x thing-y make-t t speed)
            (if (< (thing-x t) (+ BG-WIDTH SCALE))
                (make-t (+ (thing-x t) speed) (thing-y t) "right")
                (make-t (- SCALE) (thing-y t) "right")))
          ;; move-left: (X) [X -> Number] [X -> Number] (make-X Number Number String) X Number -> X
          ;; moves an item in the game to the left
          ;; vehicle-x vehicle-y make-vehicle CAR2-1 VEHICLE-SPEED ->
          ;; (make-vehicle (+ 140 VEHICLE-SPEED) ROW-3 "right")
          ;; vehicle-x vehicle-y make-vehicle (make-vehicle (- SCALE) ROW-2 "left") VEHICLE-SPEED) ->
          ;; (make-vehicle (+ BG-WIDTH SCALE) ROW-2 "left")
          (define (move-left thing-x thing-y make-t t speed)
            (if (> (thing-x t) (- SCALE))
                (make-t (- (thing-x t) speed) (thing-y t) "left")
                (make-t (+ BG-WIDTH SCALE) (thing-y t) "left")))]
    (make-world (move-player (world-player w) (world-objects w))
                (move-objects (world-objects w)))))

;; within-plank? : Player Plank -> Boolean
;; Is the player on the plank?
(check-expect (within-plank? (make-player 140 ROW-8) PLANK8-1) #t)
(check-expect (within-plank? PLAYER-INITIAL PLANK8-1) #f)

(define (within-plank? p plank)
  (<= (sqrt (+ (sqr (- (player-x p) (plank-x plank)))
               (sqr (- (player-y p) (plank-y plank)))))
      (sqrt (+ (sqr (/ PLANK-WIDTH 2))
               (sqr (/ PLANK-HEIGHT 2))))))
;; within-turtle? : Player Turtle -> Boolean
;; Is the player on the turtle?
(check-expect (within-turtle? (make-player 140 ROW-9) TURT9-1) #t)
(check-expect (within-turtle? PLAYER-INITIAL TURT9-1) #f)

(define (within-turtle? p turtle)
  (<= (sqrt (+ (sqr (- (player-x p) (turtle-x turtle)))
               (sqr (- (player-y p) (turtle-y turtle)))))
      (sqrt (+ (sqr (/ TURTLE-WIDTH 2))
               (sqr (/ TURTLE-HEIGHT 2))))))

;; move-player: World KeyEvent -> World
;; If player presses an arrow key, move in that direction
;; (otherwise no change)
(check-expect (move-player (make-world (make-player PLAYER-MIN NEAR-BOTTOM) EX-LOO) "left")
              (make-world (make-player PLAYER-MIN NEAR-BOTTOM) EX-LOO))
(check-expect (move-player (make-world (make-player 350 PLAYER-MAX-Y) EX-LOO) "down")
              (make-world (make-player 350 PLAYER-MAX-Y) EX-LOO))
(check-expect (move-player (make-world (make-player PLAYER-MAX-X NEAR-BOTTOM) EX-LOO) "right")
              (make-world (make-player PLAYER-MAX-X NEAR-BOTTOM) EX-LOO))
(check-expect (move-player (make-world (make-player PLAYER-MIN NEAR-TOP) EX-LOO) "left")
              (make-world (make-player (- PLAYER-MIN PLAYER-SPEED) NEAR-TOP) EX-LOO))
(check-expect (move-player (make-world (make-player PLAYER-MAX-X NEAR-TOP) LOO-0) "right")
              (make-world (make-player (+ PLAYER-MAX-X PLAYER-SPEED) NEAR-TOP) LOO-0))
(check-expect (move-player (make-world (make-player PLAYER-MIN NEAR-BOTTOM) EX-LOO) "up")
              (make-world (make-player PLAYER-MIN (- NEAR-BOTTOM PLAYER-SPEED)) EX-LOO))
(check-expect (move-player (make-world (make-player PLAYER-MIN NEAR-BOTTOM) LOO-0) "down")
              (make-world (make-player PLAYER-MIN (+ NEAR-BOTTOM PLAYER-SPEED)) LOO-0))
(check-expect (move-player (make-world (make-player PLAYER-MIN NEAR-BOTTOM) LOO-0) "h")
              (make-world (make-player PLAYER-MIN NEAR-BOTTOM) LOO-0))
(check-expect (move-player (make-world (make-player (+ 140 (* 3 SCALE)) (+ ROW ROW-8)) EX-LOO) "up")
              (make-world (make-player 140 ROW-8) EX-LOO))
(check-expect (move-player (make-world (make-player (+ 140 (* 3 SCALE))
                                                    (+ ROW ROW-8 50)) EX-LOO) "up")
              (make-world (make-player (+ 140 (* 3 SCALE)) (- (+ ROW ROW-8 50) PLAYER-SPEED)) EX-LOO))
(check-expect (move-player (make-world (make-player (+ 140 (* 4 SCALE)) (+ ROW ROW-8)) EX-LOO) "up")
              (make-world (make-player (+ 140 (* 4 SCALE)) (- (+ ROW ROW-8) PLAYER-SPEED)) EX-LOO))
(check-expect (move-player (make-world (make-player (- 140 (* 3 SCALE)) (- ROW-9 ROW)) EX-LOO) "down")
              (make-world (make-player 140 ROW-9) EX-LOO))
(check-expect (move-player (make-world (make-player (- 140 (* 3 SCALE))
                                                    (- ROW-9 ROW 50)) EX-LOO) "down")
              (make-world (make-player (- 140 (* 3 SCALE)) (+ (- ROW-9 ROW 50) PLAYER-SPEED)) EX-LOO))
(check-expect (move-player (make-world (make-player (- 140 (* 4 SCALE)) (- ROW-9 ROW)) EX-LOO) "down")
              (make-world (make-player (- 140 (* 4 SCALE)) (+ PLAYER-SPEED (- ROW-9 ROW))) EX-LOO))
(check-expect (move-player (make-world (make-player 140 ROW-12) EX-LOO) "up")
              (make-world (make-player 140 ROW-13) EX-LOO))

(define (move-player w key)
  (local [(define PLAYER (world-player w))
          (define LOO (world-objects w))
          ;; define move-p : Player KeyEvent LoO -> Player
          ;; move the player according to the arrow key
          ;; (make-player PLAYER-MIN 1200) "left" EX-LOO -> (make-player PLAYER-MIN 1200)
          ;; (make-player PLAYER-MIN 100) "left" EX-LOO ->
          ;; (make-player (- PLAYER-MIN PLAYER-SPEED) 100)
          
          ;; (make-player PLAYER-MAX-X 100) "right" LOO-0 ->
          ;; (make-player (+ PLAYER-MAX-X PLAYER-SPEED) 100)
          
          ;; (make-player PLAYER-MIN 1200) "up" EX-LOO ->
          ;; (make-player PLAYER-MIN (- 1200 PLAYER-SPEED))
          
          ;; (make-player PLAYER-MIN 1200) "down" LOO-0 ->
          ;; (make-player PLAYER-MIN (+ 1200 PLAYER-SPEED))
          
          ;; (make-player 0 0) "j" LOO-0 -> (make-player 0 0)
          
          (define (move-p p key loo)
            (cond [(key=? key "left") (move-p-x p loo max - PLAYER-MIN)]
                  [(key=? key "right") (move-p-x p loo min + PLAYER-MAX-X)]
                  [(key=? key "up") (move-p-y p JUMP-UP-2 max PLAYER-MIN -)]
                  [(key=? key "down") (move-p-y p JUMP-DOWN-2 min PLAYER-MAX-Y +)]
                  [else p]))
          ;; based on way planks and rivers are designed ->
          ;; JUMP-UP and JUMP-DOWN are lists of length 1
          
          ;; move-p-x : Player LoO [Number Number -> Number] [Number Number -> Number] Number ->
          ;; Player
          ;; moves the player left or right, stops at boundary if not on river
          ;; (make-player PLAYER-MIN 1200) EX-LOO  max - PLAYER-MIN -> (make-player PLAYER-MIN 1200)
          ;; (make-player PLAYER-MAX-X 100) LOO-0 min + PLAYER-MAX-X -> 
          ;; (make-player (+ PLAYER-MAX-X PLAYER-SPEED) 100)
          (define (move-p-x p loo min-or-max op p-limit)
            (if (>= (player-y p) (/ BG-HEIGHT 2))
                (make-player (min-or-max p-limit (op (player-x p) PLAYER-SPEED)) (player-y p))
                (make-player (op (player-x p) PLAYER-SPEED) (player-y p))))
          ;; move-p-y : Player [[Object -> Boolean] LoO -> LoO] [Num Num -> Num] Num [Num Num -> Num]
          ;; -> Player
          ;; moves a player up and down at normal speed, jumps onto a plank/turtle if in range
          ;; (make-player (+ 140 (* 3 SCALE)) 700)) JUMP-UP max PLAYER-MIN - ->
          ;; (make-player (+ 140 (* 3 SCALE)) (- 700 PLAYER-SPEED))
          ;; (make-player (+ 140 (* 3 SCALE)) 650)) JUMP-UP max PLAYER-MIN - -> (make-player 140 550)
          ;; (make-player (- 140 (* 3 SCALE)) 350)) JUMP-DOWN min PLAYER-MAX-Y + ->
          ;; (make-player 140 450)
          ;; (make-player 140 150) JUMP-UP max PLAYER-MIN -> (make-player 140 50)
          (define (move-p-y p jump dir p-limit op)
            (cond [(plank? jump) (make-player (plank-x jump) (plank-y jump))]
                  [(turtle? jump) (make-player (turtle-x jump) (turtle-y jump))]
                  [else
                   (if (<= (player-y PLAYER) (* 2 ROW))
                       (make-player (player-x PLAYER) (/ ROW 2))
                       (make-player (player-x p) (dir p-limit (op (player-y p) PLAYER-SPEED))))]))
          ;; within-range? : Object [Num Num -> Num] [Num Num Num -> Boolean] -> Boolean
          ;; (given (make-player (+ 140 (* 3 SCALE)) 650)) PLANK8-1 + <= -> #t
          ;; (given (make-player (+ 140 (* 3 SCALE)) 700)) PLANK8-1 + <= -> #f
          ;; (given (make-player (+ 140 (* 4 SCALE)) 650)) PLANK8-1 + <= -> #f
          ;; (given (make-player (- 140 (* 3 SCALE)) 350)) TURT9-1 - >= -> #t
          ;; (given (make-player (- 140 (* 3 SCALE)) 300)) TUR9-1 - >= -> #f
          ;; (given (make-player (- 140 (* 4 SCALE)) 350)) TURT9-1 - >=-> #f
          (define (within-range? o op dir)
            (cond [(vehicle? o) #f]
                  [(turtle? o) (and (dir (op (turtle-y o) (/ TURTLE-HEIGHT 2))
                                         (player-y PLAYER)
                                         (op (turtle-y o) ROW))
                                    (<= (- (turtle-x o) (/ TURTLE-WIDTH 2))
                                        (player-x PLAYER)
                                        (+ (turtle-x o) (/ TURTLE-WIDTH 2))))]
                  [(plank? o) (and (dir (op (plank-y o) (/ PLANK-HEIGHT 2))
                                        (player-y PLAYER)
                                        (op (plank-y o) ROW))
                                   (<= (- (plank-x o) (/ PLANK-WIDTH 2))
                                       (player-x PLAYER)
                                       (+ (plank-x o) (/ PLANK-WIDTH 2))))]))
          (define JUMP-UP (filter (λ (x) (within-range? x + <=)) LOO))
          (define JUMP-UP-2 (if (empty? JUMP-UP) 0 (first JUMP-UP))) ;; pull out result
          (define JUMP-DOWN (filter (λ (x) (within-range? x - >=)) LOO))
          (define JUMP-DOWN-2 (if (empty? JUMP-DOWN) 0 (first JUMP-DOWN)))] ;; pull out result
    (make-world (move-p (world-player w) key (world-objects w)) (world-objects w))))

;; game-over?: World -> Boolean
;; Did the player win or go down?
(check-expect (game-over? (make-world (make-player 50 ROW-13) LOO-INITIAL)) #t) ;; player won
(check-expect (game-over? (make-world (make-player 140 ROW-2) (list CAR2-1))) #t) ;; touched car
(check-expect (game-over? (make-world PLAYER-INITIAL EX-LOO)) #f) ;; game not over
(check-expect (game-over? (make-world (make-player 140 ROW-8) EX-LOO)) #f) ;; player on plank
(check-expect (game-over? (make-world (make-player 140 ROW-9) EX-LOO)) #f) ;; player on turtle
(check-expect (game-over? (make-world (make-player 400 ROW-9) EX-LOO)) #t) ;; player in river
(check-expect (game-over? (make-world (make-player (- DOT-SIZE) 50) EX-LOO)) #t) ;; player outside 
(check-expect (game-over? (make-world (make-player  (+ BG-WIDTH DOT-SIZE) 50) EX-LOO)) #t) ;; outside
(check-expect (game-over? (make-world (make-player 140 ROW-7) EX-LOO)) #f)


(define (game-over? w)
  (local [(define LOO (world-objects w))
          (define PLAYER (world-player w))
          (define PLAYER-Y (player-y PLAYER))
          (define PLAYER-X (player-x PLAYER))
          ;; player-down? : Player -> Boolean
          ;; Did the player go down?
          ;; (make-player 500 ROW-9) -> #t
          ;; (make-player 140 ROW-2) -> #t
          ;; (make-player 140 ROW-8) -> #f
          (define (player-down? p)
            (or (not (safe? p))
                (outside-bound? p)))
          ;; outside-bound?: Player -> Boolean
          ;; Did the river wash away the player?
          ;; (make-player -100 ROW-12) -> #t
          ;; (make-player (+ 100 BG-WIDTH) ROW-12) -> #t
          ;; (make-player 140 ROW-8) -> #f
          (define (outside-bound? p)
            (and (>= (* 6 ROW) PLAYER-Y ROW) ;; in river section
                 (or (>= PLAYER-X (+ BG-WIDTH DOT-SIZE)) ;; outside bounds
                     (<= PLAYER-X (- DOT-SIZE)))))
          ;; safe? : Player -> Boolean
          ;; Is the player safe? (on safe row, top, row, not touching a car or on a river object)
          ;; (make-player 0 0) -> #t
          ;; (make-player 140 ROW-7) -> #t
          ;; (make-player 500 ROW-2) -> #t
          ;; (make-player 140 ROW-2) -> #f
          ;; (make-player 140 ROW-8) -> #t
          ;; (make-player 500 ROW-8) -> #t
          (define (safe? p)
            (or (and (>= PLAYER-Y (* 6 ROW))
                     (not (ormap touching-car? LOO))) ;; not touching the cars
                (<= PLAYER-Y ROW-13)
                (and (>= (* 6 ROW) PLAYER-Y ROW)
                     (ormap in-turt-or-plank? LOO))))
          ;; in-turt--or-plank? : Object -> Boolean
          ;; Is the player on a river object?
          ;; (given (make-player 140 ROW-8)) PLANK8-1 -> #t
          ;; (given (make-player 500 ROW-9)) TURT9-1 -> #f
          (define (in-turt-or-plank? o)
            (cond [(vehicle? o) #f]
                  [(turtle? o) (within-turtle? PLAYER o)]
                  [(plank? o) (within-plank? PLAYER o)]))
          ;; touching-car? : Object -> Boolean
          ;; Is the player touching a car?
          ;; (given INITIAL-PLAYER) CAR2-1 -> #f
          ;; (given (make-player 140 ROW-2) CAR2-1 -> #t
          (define (touching-car? o)
            (and (vehicle? o)
                 (touching? PLAYER o)))
          ;; touching? : Player Vehicle -> Boolean
          ;; Is the player touching a car?
          ;; given INITIAL-PLAYER CAR2-1 -> #f
          ;; given (make-player 140 ROW-2 CAR2-1 -> #t
          (define (touching? p v)
            (<= (sqrt (+ (sqr (- (player-x p) (vehicle-x v)))
                         (sqr (- (player-y p) (vehicle-y v)))))
                (* 3 SCALE)))] ;; not perfect but erring on the side of not touching
    ;; versus touching
    (or (player-won? (world-player w))
        (player-down? (world-player w)))))

;; player-won? : Player -> Boolean
;; Did the player reach the top row?
(check-expect (player-won? (make-player 140 ROW-13)) #t)
(check-expect (player-won? (make-player 140 ROW-1)) #f)

(define (player-won? p)
  (<= (player-y p) ROW))

;; draw-end-scene: World -> Image
;; draws the end scene if the player wins or loses the game
(check-expect (draw-end-scene (make-world (make-player 50 ROW-13) LOO-INITIAL))
              (place-image PLAYER-IMG 50 ROW-13 WON-IMG))
(check-expect (draw-end-scene (make-world (make-player 140 ROW-2) (list CAR2-1)))
              (place-image PLAYER-IMG 140 ROW-2 LOST-IMG))

(define (draw-end-scene w)
  (local [(define PLAYER (world-player w))]
    (if (player-won? (world-player w))
        (place-image PLAYER-IMG (player-x PLAYER) (player-y PLAYER) WON-IMG)
        (place-image PLAYER-IMG (player-x PLAYER) (player-y PLAYER) LOST-IMG))))




