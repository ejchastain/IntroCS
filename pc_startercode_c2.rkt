;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pc_startercode_c2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/planetcute)

       ; list of lists, each list is a stack of images
 (define test_stack
   (list (list wall-block-tall stone-block)
         (list stone-block stone-block stone-block stone-block)
         (cons water-block empty)
         (list  grass-block dirt-block)
         (list  grass-block dirt-block dirt-block)
         (list   grass-block dirt-block dirt-block))
   )

(define PLAYER_IMAGE character-cat-girl)
(define STACKS test_stack)

; Pc:StructOf(Number)
; Player character information
; s_idx is stack index of the player character
(define-struct Pc [s_idx])

; Ac:StructOf(Pc,ListOf(E),ListOf(It))
; accidents of the game world
; pc is a player character struct of the accidents of the character
; loe is a list of enemy structs (accidents of the enemies)
; loi is a list of item structs (accidents of the items)
(define-struct Ac [pc loe loi])

; WS:StructOf(ListOf(ListOf(Image)))
; ac: accidents of the gameworld
(define-struct WS [ac])

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


 ; Brainstorming: 
 ; check the first within the stack so that we can find the character
 ; check all the stacks for the character
 ; keep track of the stack containing the character (character stack index)
 ; need to check if its possible to move left or right (can't move left on first stack)
 ; make new ws which allows us to place 

 ; add-image:Number x ListOf(ListOf(Image)) -> ListOf(ListOf(Image))
(define add-image
  (λ [idx stacks img]
    (cond
      [(= idx 1) (cons (cons img (first stacks)) (rest stacks))]
      [else (cons (first stacks) (add-image (sub1 idx) (rest stacks) img))]
     )
    )
  )
 
(define init-WS (make-WS (make-Ac (make-Pc 2) empty empty)))

; add-accidents:Ac -> ListOf(ListOf(Image))
(define add-accidents
  (λ [ac]
     (cond
       [(cons? (Ac-loe ac)) (add-accidents (make-Ac (Ac-pc ac) (rest (Ac-loe ac)) (Ac-loi ac)) ]
       [(cons? (Ac-loi ac)) (add-loi ...)]
    (add-image (Pc-s_idx (Ac-pc ac)) STACKS PLAYER_IMAGE)
      )
    )
  

; draw: WS -> Image
(define draw
  (λ [ws]
    (draw-stacks (add-accidents (WS-ac ws)))
    )
  )








              
    (beside/align
       "bottom"
       (stack (list wall-block-tall stone-block))
       (stack (list 
                    stone-block stone-block
                    stone-block stone-block))
       water-block
       (stack (list character-cat-girl grass-block dirt-block))
       (stack (list  grass-block dirt-block dirt-block)))

    



