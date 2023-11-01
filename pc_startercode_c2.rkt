;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pc_startercode_c2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/planetcute)

; helpers for defining the essential parts of the game world


 ; dirt_stack_hp:Number x Boolean -> ListOf(Image)
; recursively builds a stack of dirt blocks with a
; grass block on top by using the boolean dp which
; if true just means you only draw dirt blocks.
(define dirt_stack_hp
  (λ [h db]
    (if db (if (= h 1) (cons empty-image empty) (cons dirt-block (dirt_stack_hp (sub1 h) #true)))
        (cons grass-block (if (= h 1) (cons empty-image empty) (cons dirt-block (dirt_stack_hp (sub1 h) #true))))
        )
    )
  )

; dirt_stack: Number -> ListOf(Image)
; Draws a dirt stack with a grass block on top of height h. 
(define dirt_stack
  (λ [h]
    (dirt_stack_hp h #false)
    )
  )

 ; stone_stack:Number -> ListOf(Image)
; recursively builds a stack of stone blocks of height h
(define stone_stack
  (λ [h]
    (if (= h 0) (cons empty-image empty) (cons stone-block (stone_stack (sub1 h))))
    )
  )

    ; stack : ListOf(Image) -> Image
    ; stacks 'imgs' on each other, separated by 40 pixels
; from racket documentation of planet cute
(define stack
  (λ [imgs]
    (cond
      [(empty? (rest imgs)) (first imgs)]
      [else (overlay/xy (first imgs)
                        0 40
                        (stack (rest imgs)))]))
  )

; draw-stacks:ListOf(ListOf(Image)) -> Image
; draws all of the stacks in a list of stacks (a stack is a list of images)
(define draw-stacks
  (λ [loloi]
    (cond
      [(empty? loloi) empty-image]
      [else (beside/align "bottom" 
             (stack (first loloi))
             (draw-stacks (rest loloi))
             )
            ]
      )
    )
  )


       ; list of lists, each list is a stack of images, used to define our game terrain
(define test_stack
  (list (stone_stack 3)
        (stone_stack 4)
        (dirt_stack 2)
        (dirt_stack 2)
        (dirt_stack 3)
        (dirt_stack 3)
        (dirt_stack 3)
        (dirt_stack 3)
        (dirt_stack 3)
        (dirt_stack 3)
        (dirt_stack 3)         
        )
  )

; The Essential aspects of the game-world
; What does the player character look like? PLAYER_IMAGE
; What does the enemy look like? ENEMY_IMAGE
; What does the Game World terrain look like? STACKS
(define PLAYER_IMAGE character-cat-girl)
;(define ENEMY_IMAGE enemy-bug)
(define STACKS test_stack)
(define HEIGHT (* 2 (image-height (draw-stacks STACKS))))
(define WIDTH (image-width (draw-stacks STACKS)))
(define E_MOVE_TIME_DELAY 2000)

; Events in the game-world
(define-struct event [evtype init_idx goal_idx otype duration])

; set-init_idx:event x Number -> event
; Sets the init_idx field of event to the stack index new_init_idx
; and then returns the modified event
(define set-init_idx
       (λ [e new_init_idx]
               (make-event (event-evtype e) new_init_idx (event-goal_idx e) (event-evtype e) (event-duration e))
       )
)

; set-duration:event x Number -> event
; Sets the duration field of event to new_duration
; and then returns the modified event
(define set-duration
       (λ [e new_duration]
               (make-event (event-evtype e) (event-init_idx e) (event-goal_idx e) (event-evtype e) new_duration)
       )
)

; The Accidental aspects of the game-world

; Pc:StructOf(Number)
; Player character information
; s_idx is stack index of the player character
(define-struct Pc [s_idx hp])

; set-s_idx:Pc x Number -> Pc
; Sets the s_idx field of pc to the stack index new_s_idx
; and then returns the modified pc
(define set-s_idx
       (λ [pc new_s_idx]
               (make-Pc new_s_idx (Pc-hp pc))
       )
)

; set-hp:Pc x Number -> Pc
; Sets the hp field of pc to the stack index new_hp
; and then returns the modified pc
(define set-hp
       (λ [pc new_hp]
               (make-Pc  (Pc-s_idx pc) new_hp)
       )
)

; E:StructOf(Number)
; Enemy information (stack index, image for enemy)
(define-struct E [s_idx img])

; set-Es_idx:E x Number -> E
; Sets the s_idx field of E to the stack index new_s_idx
; and then returns the modified pc
(define set-Es_idx
       (λ [e new_s_idx]
               (make-E new_s_idx (E-img e))
       )
)

; It:StructOf(Number x Image)
; Item information (stack index, image for item) 
(define-struct It [s_idx img])

; Ac:StructOf(Pc,ListOf(E),ListOf(It))
; accidents of the game world
; pc is a player character struct of the accidents of the character
; loe is a list of enemy structs (accidents of the enemies)
; loi is a list of item structs (accidents of the items)
(define-struct Ac [pc loe loi time])

; set-loe:Ac x ListOf(E) -> Ac
; Sets the loe field of ac to the list of E structs new_loe
; and then returns the modified ac
(define set-loe
       (λ [ac new_loe]
               (make-Ac (Ac-pc ac) new_loe (Ac-loi ac) (Ac-time ac))
       )
)

; set-loi:Ac x ListOf(It) -> Ac
; Sets the loi field of ac to the list of It structs new_loi
; and then returns the modified ac
(define set-loi
       (λ [ac new_loi]
               (make-Ac (Ac-pc ac) (Ac-loe ac) new_loi (Ac-time ac) )
       )
)

; add1time:Ac -> Ac
; Adds 1 to the time field of ac 
; and then returns the modified ac
(define add1time
       (λ [ac]
               (make-Ac (Ac-pc ac) (Ac-loe ac) (Ac-loi ac) (add1 (Ac-time ac)) )
       )
)


; set-pc:Ac x Pc -> Ac
; Sets the pc field of ac to the Pc struct new_pc
; and then returns the modified ac
(define set-pc
       (λ [ac new_pc]
               (make-Ac new_pc (Ac-loe ac) (Ac-loi ac) (Ac-time ac))
       )
)

; move_mb: Pc|E x Number -> Pc|E
; Takes a moving body (player character or enemy) and
; moves it dir stacks over
(define move_mb
  (λ [mb dir]
    (cond
      [(Pc? mb) (set-s_idx mb (+ (Pc-s_idx mb) dir))]
      [(E? mb) (make-E (+ (E-s_idx mb) dir))]
     )
    )
  )

; move_all_E:ListOf(E) x Number -> ListOf(E)
; alternates motion from right to left or left to right for
; all enemies
(define move_all_E
  (λ [loe]
    (cond
      [(empty? loe) empty]
      [else
       (if (odd? (E-s_idx (first loe)))
           (cons (set-Es_idx (first loe) (sub1 (E-s_idx (first loe)))) (move_all_E (rest loe)))
           (cons (set-Es_idx (first loe) (add1 (E-s_idx (first loe)))) (move_all_E (rest loe)))
           )
       ]
      )
    )
  )

; jump_a: Pc|E x Number -> Pc|E
; Takes an animal (player character or enemy) and
; makes it jump up and land dir stacks over
; jump does this by making an event in the ws
(define jump_a
  (λ [ws a dir]
    (cond
      [(Pc? a) (add-event ws (make-event "jump" (Pc-s_idx a) (+ (Pc-s_idx a) dir)))]
      [(E? a) (add-event ws (make-event "jump" (E-s_idx a) (+ (E-s_idx a) dir)))]
     )
    )
  )

;TODO:
; add the frame property for large levels
; add jumping
; add events to worldstate
; add collision handler for different objects
;    - Pc and E: -1 for HP of Pc
;    - Pc and It: depends on the item... heart gives HP, gem gives points
;    - bullet and E: makes 0 HP E event, bullet collision event
;    - else leave everything as-is
; add shooting bullets (circle and a direction) event
; add generation handler
;    - generate bullets on shooting event, add move_bullet s_idx event
;    - generate new items at random after a time
;    - generate new enemies at random after a time
; add corruption handler
;     - destroy E at stack s_idx on 0 HP E event
;     - destroy Pc at stack s_idx on 0 HP Pc event
;     - destroy bullet at stack s_idx on bullet collision event
; add game_over handler
;     - end game on 0 HP Pc event

; WS:StructOf(ListOf(ListOf(Image)))
; ac: accidents of the gameworld
(define-struct WS [ac events])

; set-events:WS x ListOf(Event) -> WS
; Sets the events field of ws to the list of event structs new_events
; and then returns the modified ws
(define set-events
       (λ [ws new_events]
               (make-WS (WS-ac ws) new_events)
       )
)

; set-ac:WS x Ac -> WS
; Sets the ac field of ws to the Ac struct new_ac
; and then returns the modified ws
(define set-ac
       (λ [ws new_ac]
               (make-WS  new_ac (WS-events ws))
       )
)

; add-event:WS x event -> WS
(define add-event
  (λ [ws event]
    (set-events ws (cons event (WS-events ws)))
    )
  )

; add-E:Ac x Number x Image -> Ac
(define add-E
  (λ [ac s_idx img]
    (set-loe ac (cons (make-E s_idx img) (Ac-loe ac)))
    )
  )

; add-It:Ac x Number x Image -> Ac
(define add-It
  (λ [ac s_idx img]
    (set-loe ac (cons (make-E s_idx img) (Ac-loe ac)))
    )
  )

; The Initial World State
;  Place the player character on stack 2,
;  enemy bug on stack 4, enemy horn girl
;  on stack 6
(define initial-WS (make-WS (make-Ac (make-Pc 1 1)
                                  (list (make-E 4 enemy-bug) (make-E 6 character-horn-girl))
                                  (list (make-It 8 gem-orange) (make-It 2 gem-blue)) 0) empty))


; Helper functions for draw


 ; add-image:Number x ListOf(ListOf(Image)) -> ListOf(ListOf(Image))
; add-image just adds an image at stack index idx to the list of lists of images stacks
(define add-image
  (λ [idx stacks img]
    (cond
      [(= idx 1) (cons (cons img (first stacks)) (rest stacks))]
      [else (cons (first stacks) (add-image (sub1 idx) (rest stacks) img))]
     )
    )
  )
 
; add-accidents:Ac x ListOf(ListOf(Image)) -> ListOf(ListOf(Image))
; draws all of the accidents ac on the terrain stacks stacks 
; Approach: as suggested by ben anderson, try to recurse on the non-empty lists
; of enemies and lists of items, adding the different objects to the terrain (at the
; corresponding stack index s_idx) as you go. Then after you are done with the list 
; fields, draw the player character. 
(define add-accidents
  (λ [ac stacks]
     (cond
       [(cons? (Ac-loe ac))
          (add-accidents (set-loe ac (rest (Ac-loe ac)))
                         (add-image
                          (E-s_idx (first (Ac-loe ac))) stacks
                          (E-img (first (Ac-loe ac)))))]
       [(cons? (Ac-loi ac))
          (add-accidents (set-loi ac (rest (Ac-loi ac)))
                         (add-image
                          (It-s_idx (first (Ac-loi ac))) stacks
                          (It-img (first (Ac-loi ac))))
                         )]
     ;  [(cons? (Ac-loi ac)) (add-accidents (set-loe ac (rest (Ac-loe ac))))]     
    [else (add-image (Pc-s_idx (Ac-pc ac)) stacks PLAYER_IMAGE)]
      )
    )
  )

; draw: WS -> Image
; draws the worldstate ws by outputting its corresponding picture as output
; Approach: Use add-accidents with the essential STACKS game-world as the background
; add-accidents will just add all of the different images from the accidents structure
; to the stacks list of lists of images to get the final image to draw. 
(define draw
  (λ [ws]
    (place-image  
     (draw-stacks (add-accidents (WS-ac ws) STACKS))
     (/ WIDTH 2)
     (- HEIGHT 200)
     (rectangle WIDTH HEIGHT "solid" "light blue")
    )
  )
  )

; WorldState -> WorldState
(define tock
  (λ  [ws]
    (cond
      [(= (modulo (Ac-time (WS-ac ws)) E_MOVE_TIME_DELAY) 0) 
       (set-ac ws (add1time (set-loe (WS-ac ws) (move_all_E (Ac-loe (WS-ac ws))))))
       ]
      [else (set-ac ws (add1time (WS-ac ws)))]
      )
    )
  )


; key-handler: WS x Key -> WS
; handles key presses
(define key-handler
  (λ [ws a-key]
    (cond
      [(key=? a-key "right") (set-ac ws (set-pc (WS-ac ws) (move_mb (Ac-pc (WS-ac ws)) 1)))]
      [(key=? a-key "left") (set-ac ws (set-pc (WS-ac ws) (move_mb (Ac-pc (WS-ac ws)) -1)))]
      [(key=? a-key "up") (jump_a (Ac-pc (WS-ac ws)) 2)]
      [(key=? a-key " ") (jump_a (Ac-pc (WS-ac ws)) -2)]
      [else ws])
    )
  )

; playerDead?:WS->Boolean
; Checks if the player character has 0 HP
(define playerDead? 
  (λ [ws]
     (= (Pc-hp (Ac-pc (WS-ac ws))) 0)
    )
)

; game-over:WS->Image
; draws a game-over screen
(define (game-over ws)
  (place-image (text "Game Over" 64 "white") (/ WIDTH 2) (/ HEIGHT 2) (rectangle WIDTH HEIGHT "solid" "black")))

    

(big-bang initial-WS
  (to-draw draw) ; for drawing
  (on-key key-handler) ; to respond to keyboard activity 
  (on-tick tock (/ 1 1000)) ; runs tock every tick
  (stop-when playerDead? game-over) ; draws game over screen when player character is dead
  )

