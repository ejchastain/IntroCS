;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pc_startercode_c2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/planetcute)

       ; list of lists, each list is a stack of images, used to define our game terrain
 (define test_stack
   (list (list wall-block-tall stone-block)
         (list stone-block stone-block stone-block stone-block)
         (cons water-block empty)
         (list  grass-block dirt-block)
         (list  grass-block dirt-block dirt-block)
         (list   grass-block dirt-block dirt-block))
   )

; The Essential aspects of the game-world
; What does the player character look like? PLAYER_IMAGE
; What does the enemy look like? ENEMY_IMAGE
; What does the Game World terrain look like? STACKS
(define PLAYER_IMAGE character-cat-girl)
(define ENEMY_IMAGE enemy-bug)
(define STACKS test_stack)

; The Accidental aspects of the game-world

; Pc:StructOf(Number)
; Player character information
; s_idx is stack index of the player character
(define-struct Pc [s_idx])

; E:StructOf(Number)
; Enemy information
(define-struct E [s_idx])

; It:StructOf(Number x Image)
; Item information (stack index, image item) 
(define-struct It [s_idx img])

; Ac:StructOf(Pc,ListOf(E),ListOf(It))
; accidents of the game world
; pc is a player character struct of the accidents of the character
; loe is a list of enemy structs (accidents of the enemies)
; loi is a list of item structs (accidents of the items)
(define-struct Ac [pc loe loi])

; set-loe:Ac x ListOf(E) -> Ac
; Sets the loe field of ac to the list of E structs new_loe
; and then returns the modified ac
(define set-loe
       (λ [ac new_loe]
               (make-Ac (Ac-pc ac) new_loe (Ac-loi ac))
       )
)

; set-loi:Ac x ListOf(It) -> Ac
; Sets the loi field of ac to the list of It structs new_loi
; and then returns the modified ac
(define set-loi
       (λ [ac new_loi]
               (make-Ac (Ac-pc ac) (Ac-loe ac) new_loi )
       )
)

; set-pc:Ac x Pc -> Ac
; Sets the pc field of ac to the Pc struct new_pc
; and then returns the modified ac
(define set-pc
       (λ [ac new_pc]
               (make-Ac new_pc (Ac-loe ac) (Ac-loe ac) )
       )
)


; WS:StructOf(ListOf(ListOf(Image)))
; ac: accidents of the gameworld
(define-struct WS [ac])

; The Initial World State
(define init-WS (make-WS (make-Ac (make-Pc 2) (list (make-E 3) (make-E 6)) empty)))


; Helper functions for draw

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
; draws all of the accidents ac on the terrain stacks stks 
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
                          (E-s_idx (first (Ac-loe ac))) stacks ENEMY_IMAGE))]
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
    (draw-stacks (add-accidents (WS-ac ws) STACKS))
    )
  )
    



