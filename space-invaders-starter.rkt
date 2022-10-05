;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |space-invaders-starter - コピー|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define INVADER-DX-TEST 12)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer



(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body


(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Constants for testing:

(define TANK-ON-BG (place-image TANK
                                (/ WIDTH 2)
                                HEIGHT
                                BACKGROUND))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number[0, WIDTH] Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1, no movement if dir 0

(define T0 (make-tank (/ WIDTH 2) 0))   ;center not moving
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T3 (make-tank 50 0))            ;not moving
(define T4 (make-tank 0 0))             ;tank at left edge of the screen
(define T5 (make-tank WIDTH 0))         ;tank at right edge of the screen

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 150 100 INVADER-X-SPEED))  


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders
(define LOINVADER1 empty)
(define LOVINVADER2 (cons (make-invader 150 100 12)
                          (cons (make-invader 150 120 -10)
                                empty)))
#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) ...]
        [else
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first loinvader) is Invader
;; - self-reference: (rest loinvader) is ListOfInvader


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissle is one of:
;; - empty
;; - (cons Missle ListOfMissle)
;; interp. a list of missles
(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300)
                   (cons (make-missile 200 350)
                         empty)))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) ...]
        [else
         (... (fn-for-missle (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first lom) is Missle
;; - self-reference: (rest lom) is ListOfMissle


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (cons I4 empty) empty T0))


;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank (/ WIDTH 2) 1))
;; 
(define (main s)
  (big-bang s                        ; Game
    (on-tick   tock)         ; Game -> Game
    (to-draw   render)       ; Game -> Image
    (stop-when end-game?)    ; Game -> Boolean
    (on-key    handle-key))) ; Game KeyEvent -> Game


;; Game -> Game
;; produces the next game state
(check-expect (tock (make-game empty empty (make-tank (/ WIDTH 2) 0)))            ;tank not moving
              (make-game empty empty (make-tank (/ WIDTH 2) 0)))
(check-expect (tock (make-game empty empty (make-tank (/ WIDTH 2) 1)))            ;tank moving right
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (tock (make-game empty empty (make-tank (/ WIDTH 2) -1)))           ;tank moving left
              (make-game empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-expect (tock (make-game                                                    ;tick missles and invaders
                     (cons (make-invader 150 100 INVADER-DX-TEST)
                           (cons (make-invader 150 100 (- INVADER-DX-TEST))
                                 empty))
                     (cons (make-missile 150 300)
                           (cons (make-missile 200 200)
                                 empty))
                     (make-tank (/ WIDTH 2) 0)))
              (make-game
               (cons (make-invader (+ 150 INVADER-DX-TEST)
                                   (+ INVADER-Y-SPEED 100)
                                   INVADER-DX-TEST)
                     (cons (make-invader (- 150 INVADER-DX-TEST)
                                         (+ INVADER-Y-SPEED 100)
                                         (- INVADER-DX-TEST))
                           empty))
               (cons (make-missile 150 (- 300 MISSILE-SPEED))
                     (cons (make-missile 200 (- 200 MISSILE-SPEED))
                           empty))
               (make-tank (/ WIDTH 2) 0)))
                     
;(define (tock s) ...) ; stub
;<template from Game>
(define (tock s)
  (make-game (check-collision (spawn-invaders (tick-loinvader (game-invaders s)))
                              (game-missiles s))
             (remove-offscreen-lom (tick-lom (game-missiles s)))
             (fix-offscreen-tank (tick-tank (game-tank s)))))


;; ListOfInvader -> ListOfInvader
;; move list of invaders down the screen by increasing x by given dx and and y by INVADER-Y-SPEED, reverse direction of the invader if it moves past left or right screen border and set invader x back to the edge of the border
(check-expect (tick-loinvader empty) empty)
(check-expect (tick-loinvader (cons (make-invader 200 300 INVADER-X-SPEED)
                                    (cons (make-invader 100 150 INVADER-X-SPEED)
                                          empty)))
              (cons (make-invader (+ 200 INVADER-X-SPEED)(+ 300 INVADER-Y-SPEED) INVADER-X-SPEED)
                    (cons (make-invader (+ 100 INVADER-X-SPEED)(+ 150 INVADER-Y-SPEED) INVADER-X-SPEED)
                          empty)))
;(define (tick-loinvader loinvader) loinvader) ; stub
;<template from ListOfInvader>
(define (tick-loinvader loinvader)
  (cond [(empty? loinvader) empty]
        [else
         (cons (fix-offscreen-invader (tick-invader (first loinvader)))
               (tick-loinvader (rest loinvader)))]))


;; Invader -> Invader
;; move given invader down the screen by increasing x by given dx and and y by INVADER-Y-SPEED
(check-expect (tick-invader (make-invader 200 300 INVADER-X-SPEED))
              (make-invader (+ 200 INVADER-X-SPEED)(+ 300 INVADER-Y-SPEED) INVADER-X-SPEED))
;(define (tick-invader i) i) ;stub
;<template from Invader>
(define (tick-invader invader)
  (make-invader (+ (invader-x invader) (invader-dx invader))
                (+ (invader-y invader) INVADER-Y-SPEED)
                (invader-dx invader)))


;; Invader -> Invader
;; reverse invader direction if invader x pos < 0 or x > WIDTH, and move invader back to the edge of the border it crossed (back to 0 or WIDTH)
(check-expect (fix-offscreen-invader (make-invader 10 200 INVADER-X-SPEED))  ; WIDTH > x > 0, no change
              (make-invader 10 200 INVADER-X-SPEED))
(check-expect (fix-offscreen-invader (make-invader -1 200 (- INVADER-X-SPEED)))  ; x < 0, change dir from left to right, correct pos to 0
              (make-invader 0 200 (- (- INVADER-X-SPEED))))
(check-expect (fix-offscreen-invader (make-invader (+ WIDTH 1) 200 INVADER-X-SPEED))  ; x > WIDTH, change dir from left to right, correct pos to WIDTH
              (make-invader WIDTH 200 (- INVADER-X-SPEED)))
;(define (fix-offscreen-invader i) i) ;stub
;<template from Invader>
(define (fix-offscreen-invader invader)
  (cond [(> (invader-x invader) WIDTH) (make-invader WIDTH
                                                     (invader-y invader)
                                                     (- (invader-dx invader)))]
        [(< (invader-x invader) 0) (make-invader 0
                                                 (invader-y invader)
                                                 (- (invader-dx invader)))]
        [else invader]))

;; ListOfInvader -> ListOfInvader
;; randomly spawn new invaders at random x location and y-pos 0
(check-expect (spawn-invaders empty)                      ;add an invader at the middle of the x-axis
              (cons (make-invader (/ WIDTH 2) 0 INVADER-X-SPEED) empty))
;(define (spawn-invaders lom) lom) ; stub
;<template from ListOfInvader>
(define (spawn-invaders loinvader)
  (if (> (random 100) 98)
      (cons (make-invader (random (+ WIDTH 1)) 0 INVADER-X-SPEED) loinvader)
      loinvader))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; checks if any invader collides with any missile by comparing their x and y positions (with leniency of +- HIT-RANGE) and removes the invader that overlaps with a missile
(check-expect (check-collision empty empty) empty)
(check-expect (check-collision (cons (make-invader 150 100 12) empty)
                               (cons (make-missile 100 100) empty))
              (cons (make-invader 150 100 12) empty))
(check-expect (check-collision (cons (make-invader 200 300 12)
                                     (cons (make-invader 150 100 12)
                                           empty))
                               (cons (make-missile 50 20)
                                     (cons (make-missile 150 100)
                                           empty)))
              (cons (make-invader 200 300 12) empty))
(check-expect (check-collision (cons (make-invader 200 300 12)
                                     (cons (make-invader 150 100 12)
                                           empty))
                               (cons (make-missile 50 20)
                                     (cons (make-missile (+ 140 HIT-RANGE)
                                                         (+ 100 HIT-RANGE))
                                           empty)))
              (cons (make-invader 200 300 12) empty))

;(define (check-collision loinvader lom) loinvader) ; stub
;<template from ListOfInvader with added ListOfMissile parameter and function composition helper>
(define (check-collision loinvader lom)
  (cond [(empty? loinvader) loinvader]
        [else
         (if (collision? (first loinvader)
                         lom)
             (check-collision (rest loinvader) lom)
             (cons (first loinvader) (check-collision (rest loinvader) lom)))]))


;; Invader ListOfMissile -> Boolean
;; returns true if the x, y position (with leniency of +- HIT-RANGE) of the given invader overlaps with the x,y pos of any missile
(check-expect (collision? (make-invader 100 100 12)
                          empty)
              false)
(check-expect (collision? (make-invader 100 100 12)
                          (cons (make-missile 80 80)
                                (cons (make-missile 120 120)
                                      empty)))
              false)
(check-expect (collision? (make-invader 100 100 12)
                          (cons (make-missile 80 80)
                                (cons (make-missile 110 110)
                                      empty)))
              true)

;(define (collision? invader lom) false) ; stub
;<template from Invader with added ListOfMissile parameter>
;<template from ListOfMissile with added Invader parameter>
(define (collision? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (invader-x invader)
                              (missile-x (first lom))))
                      HIT-RANGE)
                  (<= (abs (- (invader-y invader)
                              (missile-y (first lom))))
                      HIT-RANGE))
             true
             (collision? invader (rest lom)))]))

;; ListOfMissle -> ListOfMissle
;; updates position of given list of missiles by moving their y-pos up the screen (towards y-pos 0) by MISSILE-SPEED
(check-expect (tick-lom empty) empty)                               ;no missles
(check-expect (tick-lom (cons (make-missile 100 200)                ;tick several missiles
                              (cons (make-missile 150 150)
                                    empty)))
              (cons (make-missile 100 (- 200 MISSILE-SPEED))
                    (cons (make-missile 150 (- 150 MISSILE-SPEED))
                          empty)))
;(define (tick-lom lom) lom) ; stub
;<template from ListOfMissile>
(define (tick-lom lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile (first lom))
               (tick-lom (rest lom)))]))


;; Missile -> Missile
;; updates position of missile by increasing its y-pos by MISSILE-SPEED
(check-expect (tick-missile (make-missile 150 200))
              (make-missile 150 (- 200 MISSILE-SPEED)))
;(define (tick-missile m) m) ; stub
;<template from Missile>
(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissile -> ListOfMissile
;; remove missiles from list that are offscreen (y-pos < 0)
(check-expect (remove-offscreen-lom empty) empty)
(check-expect (remove-offscreen-lom (cons (make-missile 100 -5)   ; remove y < 0
                                          empty))
              empty)
(check-expect (remove-offscreen-lom (cons (make-missile 100 0)    ; don't remove y = 0
                                          empty))
              (cons (make-missile 100 0)
                    empty))
(check-expect (remove-offscreen-lom
               (cons (make-missile 100 200)                
                     (cons (make-missile 150 -5)
                           empty)))
              (cons (make-missile 100 200)                
                    empty))

;(define (remove-offscreen-lom lom) lom) ; stub
;<template from ListOfMissile>
(define (remove-offscreen-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (offscreen-missile? (first lom))
             (remove-offscreen-lom (rest lom))
             (cons (first lom) (remove-offscreen-lom (rest lom))))]))


;; Missile -> Boolean
;; returns true if a missile is off-screen (y-pos < 0)
(check-expect (offscreen-missile? (make-missile 150 200))
              false)
(check-expect (offscreen-missile? (make-missile 150 0))
              false)
(check-expect (offscreen-missile? (make-missile 150 -5))
              true)
;(define (offscreen-missile? m) false) ; stub
;<template from Missile>
(define (offscreen-missile? m)
  (< (missile-y m) 0))


;; Tank -> Tank
;; moves tank to the left by TANK-SPEED if tank-dir is -1, to the right if 1, and no movement if dir is 0
(check-expect (tick-tank (make-tank (/ WIDTH 2) 0))       ;not moving
              (make-tank (/ WIDTH 2) 0))
(check-expect (tick-tank (make-tank (/ WIDTH 2) 1))       ;moving right
              (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1))
(check-expect (tick-tank (make-tank (/ WIDTH 2) -1))      ;moving left
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
;(define (tick-tank t) t) ; stub
;<template from Tank>
(define (tick-tank t)
  (cond[(= (tank-dir t) 1)
        (make-tank (+ TANK-SPEED (tank-x t)) (tank-dir t))]
       [(= (tank-dir t) -1)
        (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]
       [else
        (make-tank (tank-x t) (tank-dir t))]))


;; Tank -> Tank
;; if tank goes offscreen (x < 0, x > WIDTH), correct tank position to edge of screen border (0 or WIDTH) and stop movement (dir is 0)
(check-expect (fix-offscreen-tank (make-tank 50 1))
              (make-tank 50 1))
(check-expect (fix-offscreen-tank (make-tank -5 -1))
              (make-tank 0 0))
(check-expect (fix-offscreen-tank (make-tank (+ 5 WIDTH) 1))
              (make-tank WIDTH 0))
;(define (fix-offscreen-tank t) t) ; stub
;<template from Tank>
(define (fix-offscreen-tank t)
  (cond [(< (tank-x t) 0)
         (make-tank 0 0)]
        [(> (tank-x t) WIDTH)
         (make-tank WIDTH 0)]
        [else t]))


;; Game -> Image
;; render the tank, list of missiles, and list of invaders in the game state at their correct positions
(check-expect (render (make-game                       
                       empty
                       empty
                       (make-tank (/ WIDTH 2) 0)))
              (place-image empty-image
                           0
                           0
                           (place-image empty-image
                                        0
                                        0
                                        (place-image TANK
                                                     (/ WIDTH 2)
                                                     HEIGHT
                                                     BACKGROUND))))
;(define (render s) empty-image) ; stub
;<template from Game>
(define (render s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; ListOfInvader Image -> Image
;; render a list of invaders on top of the given image
(check-expect (render-invaders empty TANK-ON-BG)
              TANK-ON-BG)
(check-expect (render-invaders (cons (make-invader 150 100 12)
                                     (cons (make-invader 150 120 -10)
                                           empty))
                               TANK-ON-BG)
              (place-image INVADER
                           150
                           100
                           (place-image INVADER
                                        150
                                        120
                                        TANK-ON-BG)))
;(define (render-invaders loinvader i) empty-image) ;stub
;<template from ListOfInvader, with added atomic parameter>
(define (render-invaders loinvader i)
  (cond [(empty? loinvader) i]
        [else
         (place-image INVADER
                      (invader-x (first loinvader))
                      (invader-y (first loinvader))
                      (render-invaders (rest loinvader) i))]))


;; ListOfMissile -> Image
;; render a list of missiles on top of the given image
(check-expect (render-missiles empty TANK-ON-BG) TANK-ON-BG)
(check-expect (render-missiles (cons (make-missile 150 300)
                                     (cons (make-missile 200 350)
                                           empty))
                               TANK-ON-BG)
              (place-image MISSILE
                           150
                           300
                           (place-image MISSILE
                                        200
                                        350
                                        TANK-ON-BG)))
;(define (render-missiles lom i) empty-image) ;stub
;<template from ListOfMissile, with added atomic parameter>
(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) i))]))


;; Tank -> Image
;; render the TANK image on BACKGROUND with tank-x as the x pos and HEIGHT as the y pos
(check-expect (render-tank (make-tank (/ WIDTH 2) 0))
              (place-image TANK
                           (/ WIDTH 2)
                           HEIGHT
                           BACKGROUND))
;(define (render-tank t) empty-image) ;stub
;<template from Tank>
(define (render-tank t)
  (place-image TANK
               (tank-x t)
               HEIGHT
               BACKGROUND))


;; Game KeyEvent -> Game
;; shoot missles with space key, move ship left/right with left/right keys
;; left key changes ship dir to -1, right key changes dir to 1
(check-expect (handle-key (make-game                       ;left 
                           empty
                           empty
                           (make-tank (/ WIDTH 2) 0))
                          "left")  
              (make-game empty
                         empty
                         (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game                       ;right  
                           empty
                           empty
                           (make-tank (/ WIDTH 2) 0))
                          "right")  
              (make-game empty
                         empty
                         (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game                       ;shoot missile with space bar
                           empty
                           empty
                           (make-tank (/ WIDTH 2) 0))
                          " ")  
              (make-game empty
                         (cons (make-missile (/ WIDTH 2)
                                             (- HEIGHT (image-height TANK)))
                               empty)
                         (make-tank (/ WIDTH 2) 0)))
(check-expect (handle-key (make-game                       ;no change with any other key presses
                           empty
                           empty
                           (make-tank (/ WIDTH 2) 0))
                          "q")  
              (make-game empty
                         empty
                         (make-tank (/ WIDTH 2) 0)))
(define (handle-key s ke)
  (cond [(key=? ke "left") (make-game (game-invaders s)
                                      (game-missiles s)
                                      (make-tank (tank-x (game-tank s))
                                                 -1))]
        [(key=? ke "right") (make-game (game-invaders s)
                                       (game-missiles s)
                                       (make-tank (tank-x (game-tank s))
                                                  1))]
        [(key=? ke " ") (make-game (game-invaders s)
                                   (add-missile (game-missiles s) (tank-x (game-tank s)))
                                   (game-tank s))]
        [else s]))


;; ListOfMissile Number -> ListOfMissile
;; add a missile to the list with x-pos of given number, and y-pos of HEIGHT subtraced by height of the ship image
(check-expect (add-missile empty 20)
              (cons (make-missile 20 (- HEIGHT (image-height TANK)))
                    empty))
;(define (add-missile lom x) lom) ; stub
;<template from ListOfMissile with added atomic parameter>
(define (add-missile lom x)
  (cons (make-missile x (- HEIGHT (image-height TANK)))
        lom))


;; Game -> Boolean
;; ends the game when an invader touches the bottom of the screen (y position > HEIGHT)
(check-expect (end-game? (make-game
                          (cons (make-invader 170 100 INVADER-DX-TEST)
                                (cons (make-invader 200 300 INVADER-DX-TEST)
                                      empty))
                          (cons (make-missile 150 300)
                                (cons (make-missile 50 200)
                                      empty))
                          (make-tank (/ WIDTH 2) 0)))
              false)
(check-expect (end-game? (make-game
                          (cons (make-invader 170 100 INVADER-DX-TEST)
                                (cons (make-invader 200 HEIGHT INVADER-DX-TEST)
                                      empty))
                          (cons (make-missile 150 300)
                                (cons (make-missile 50 200)
                                      empty))
                          (make-tank (/ WIDTH 2) 0)))
              false)
(check-expect (end-game? (make-game
                          (cons (make-invader 170 100 INVADER-DX-TEST)
                                (cons (make-invader 200 (+ 1 HEIGHT) INVADER-DX-TEST)
                                      empty))
                          (cons (make-missile 150 300)
                                (cons (make-missile 50 200)
                                      empty))
                          (make-tank (/ WIDTH 2) 0)))
              true)              
;(define (end-game? s) false) ; stub
;<template from Game>
(define (end-game? s)
  (invader-landed? (game-invaders s)))


;; ListOfInvader -> Boolean
;; produce true if the position of any invaders are y > HEIGHT
(check-expect (invader-landed? (cons (make-invader 170 100 INVADER-DX-TEST)
                                (cons (make-invader 200 HEIGHT INVADER-DX-TEST)
                                      empty)))
              false)
(check-expect (invader-landed? (cons (make-invader 170 100 INVADER-DX-TEST)
                                (cons (make-invader 200 (+ 1 HEIGHT) INVADER-DX-TEST)
                                      empty)))
              true)
;(define (invader-landed? loinvader) false) ; stub
;<template from ListOfInvader>
(define (invader-landed? loinvader)
  (cond [(empty? loinvader) false]
        [else
         (if (> (invader-y (first loinvader))
                HEIGHT)
              true
              (invader-landed? (rest loinvader)))]))


