;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris-223) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101
; Assignment 5
; tetris-223.rkt
; <Zoe Alberga>

(require 2htdp/image)
(require 2htdp/universe)

#|
Exercise 223. Equip the program from exercise 222 with a stop-when clause.
The game ends when one of the columns contains enough blocks to “touch”
the top of the canvas.

Once you have solved exercise 223, you have a bare-bones Tetris game. You
may wish to polish it a bit before you show it to your friends. For example,
the final canvas could display a text that says how many blocks the player
was able to stack up. Or every canvas could contain such a text. The choice
is yours.
|#
; Copy code from exercise 220 as your starting point to work on exercise 221
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 10)     ; # of blocks, horizontally 
(define HEIGHT WIDTH) ; the maximal number of blocks vertically

; graphical constants 
(define SIZE 40)      ; blocks are square 
(define BLOCK         ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "Pale Turquoise")
    (square SIZE "outline" "Light Sky Blue"))) 

(define SCENE-SIZE (* WIDTH SIZE))
(define MT-SCENE
  (overlay (rectangle (- SCENE-SIZE 1) (- SCENE-SIZE 1) "solid" "white")
           (rectangle SCENE-SIZE SCENE-SIZE "solid" "black")))

; data and structure definitions:

; A Tetris is a structure:
;   (make-tetris Block Landscape)
(define-struct tetris [block landscape])

; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)

; A Block is a structure:
;   (make-block N N)
(define-struct block [x y])
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting 

; Data examples
(define landscape0 '())
(define block-0-0 (make-block 0 0))
(define block-0-1 (make-block 0 1))
(define block-dropping block-0-0)
(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-drop (make-tetris block-0-1 landscape0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape1 (list block-landed))
(define landscape2 (list block-on-block block-landed))
(define tetris1 (make-tetris block-dropping landscape2))
(define landscape3 (list (make-block 4 (- HEIGHT 1)) (make-block 4 (- HEIGHT 2))))

; Functions

; Block -> number
; returns physical x-coordinate of given block
(define (physical-x b)
  (+ (/ SIZE 2) (* SIZE (block-x b))))

(check-expect (physical-x block-0-0) (+ (/ SIZE 2) 0)) 
(check-expect (physical-x (make-block 9 0)) (+ (/ SIZE 2) (* SIZE 9)))

; Block -> number
; returns physical y-coordinate of given block
(define (physical-y b)
  (+ (/ SIZE 2) (* SIZE (block-y b))))

(check-expect (physical-y block-0-0) (+ (/ SIZE 2) 0))
(check-expect (physical-y (make-block 0 9)) (+ (/ SIZE 2) (* SIZE 9)))

; Landscape -> image
; renders image of given Landscape
(define (blocks-render blocks) 
  (cond
    [(empty? blocks) MT-SCENE]
    [(cons? blocks) (place-image BLOCK
                                 (physical-x (first blocks)) 
                                 (physical-y (first blocks)) 
                                 (blocks-render (rest blocks)))]))

(check-expect (blocks-render '()) MT-SCENE)
(check-expect (blocks-render landscape1)
              (place-image BLOCK
                           (physical-x block-landed)
                           (physical-y block-landed)
                           MT-SCENE))

; Tetris -> image
; renders image of given Tetris game
(define (tetris-render t)
  (blocks-render (cons (tetris-block t) (tetris-landscape t))))

; Examples
(tetris-render tetris0)
(tetris-render tetris0-drop)
(tetris-render tetris1)

(check-expect (tetris-render tetris0)
              (place-image BLOCK
                           (physical-x block-dropping)
                           (physical-y block-dropping)
                           MT-SCENE))
(check-expect (tetris-render tetris0-drop)
              (place-image BLOCK
                           (physical-x block-0-1)
                           (physical-y block-0-1)
                           MT-SCENE))
(check-expect (tetris-render tetris1)
              (place-image BLOCK
                           (physical-x block-dropping)
                           (physical-y block-dropping)
                           (blocks-render (tetris-landscape tetris1))))



;BLOCK -> BLOCK
;change the y corridinate of the block 
(define (drop-block r)
(make-block (block-x r) (+ (block-y r) 1))) 

(check-expect (drop-block (make-block 3 3)) (make-block 3 4))

;Tetris -> boolean
;to check is a block landed on the bottom or on another block 
(define (landed r)
(cond
[(= (block-y (tetris-block r)) 9) #true]
[(member? (drop-block (tetris-block r)) (tetris-landscape r)) #true]
[else #false]))

(check-expect (landed (make-tetris (make-block 0 (- HEIGHT 2)) landscape1)) #true)
(check-expect (landed tetris0) #false)
                   


 ;BLOCK -> BLOCK
 ;Generate a new block to the right of the old block 
 (define (block-generate r)
   (make-block (modulo (+ (block-x r) 1) WIDTH) 0 ))

(check-expect (block-generate (make-block 0 9)) (make-block 1 0)) 

 ;Tetris -> Tetris
 ;Fucntion that drops a block determines if it has landed and if it has it will drop a new block 
(define (tetris-tick r)
  (cond
    [(landed r) (make-tetris (block-generate (tetris-block r)) (cons (tetris-block r)(tetris-landscape r)))]
    [else (make-tetris (drop-block (tetris-block r)) (tetris-landscape r))]))

(check-expect (tetris-tick (make-tetris block-0-0 landscape0)) (make-tetris block-0-1 landscape0))
(check-expect (tetris-tick (make-tetris (make-block 0 9) landscape0)) (make-tetris (make-block 1 0) landscape1)) 

;Tetris key -> Tetris
;move the block to the left or to the right based on the key that is pressed
(define (tetris-key r a-key)
  (cond  
    
      [(and (not(or (block-right r) (block-wall-right r))) (key=? a-key "right")) (make-tetris (make-block (+ (block-x (tetris-block r)) 1) (block-y (tetris-block r))) (tetris-landscape r))]
  
     [(and (not (or (block-left r) (block-wall-left r))) (key=? a-key "left")) (make-tetris (make-block (- (block-x (tetris-block r)) 1) (block-y (tetris-block r))) (tetris-landscape r))]
  [else r]))
   

(check-expect (tetris-key (make-tetris (make-block 3 (- HEIGHT 2)) landscape1) "left") (make-tetris (make-block 2 (- HEIGHT 2)) landscape1))
(check-expect (tetris-key (make-tetris (make-block 0 (- HEIGHT 2)) landscape1) "right") (make-tetris (make-block 1 (- HEIGHT 2)) landscape1))


;Tetris -> bool
;Determines if a block is against on the the right wall
(define (block-right r)
  (= (block-x (tetris-block r)) 9))

(check-expect (block-right (make-tetris (make-block 9 4) landscape0)) #true)
(check-expect (block-right (make-tetris (make-block 0 4) landscape0)) #false)

;Tetris-> bool 
;Determines if a block is against the left wall
(define (block-left r) 
  (= (block-x (tetris-block r)) 0)) 

(check-expect (block-left (make-tetris (make-block 0 4) landscape0)) #true)
(check-expect (block-left (make-tetris (make-block 9 4) landscape0)) #false)

;Tetris -> bool
;Determines if a block has a wall of blocks against its left side
(define (block-wall-left r)
  (member? (make-block (- (block-x (tetris-block r)) 1) (block-y (tetris-block r))) (tetris-landscape r)))

(check-expect (block-wall-left (make-tetris (make-block 1 (- HEIGHT 2)) landscape2)) #true)
(check-expect (block-wall-left (make-tetris (make-block 2 (- HEIGHT 2)) landscape2)) #false)

;Tetris -> bool
;Determines if a block has a wall aginst its right side
(define (block-wall-right r)
  (member? (make-block (+ (block-x (tetris-block r)) 1) (block-y (tetris-block r))) (tetris-landscape r)))

(check-expect (block-wall-right (make-tetris (make-block 3 (- HEIGHT 2)) landscape3)) #true)
(check-expect (block-wall-right (make-tetris (make-block 2 (- HEIGHT 2)) landscape3)) #false)
  

;Tetris -> bool
;To see if any of the blocks in the landscape are at y equals to zero
(define (game-over? r)
  (y-zero? (tetris-landscape r)))
  

;List-of-blocks -> bool
;Determins if a lost of blocks has any blocks with a y-cooridnate of 0  
 (define (y-zero? r) 
  (cond
    [(empty? r) #false]
     [(cons? r)
 (or (blocky? (first r))
  (y-zero? (rest r)))]))

  (check-expect (y-zero? '()) #false)
  (check-expect (y-zero? (list(make-block 1 2) (make-block 1 0))) #true) 


;block -> bool
;determine if the y value  of a block is equal to zero
(define (blocky? r)
  (= (block-y r) 0))  

(check-expect (blocky? (make-block 2 0)) #true)
(check-expect (blocky? (make-block 1 2)) #false)


   

;Tetris -> Text
;Display Game over and the final score of the game
(define (final-render r)
  (overlay (above (text "Game Over" 20 "Slate Blue") (beside (text "Score: " 15 "Violet Red") (text (number->string (length (tetris-landscape r))) 15 "Violet Red"))) (tetris-render r)))     

; Copy code from exercise 222 as your starting point to work on exercise 223

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(define (tetris-main r)
  (big-bang tetris0
            (to-draw tetris-render)
            (on-tick tetris-tick r) 
            (on-key tetris-key)
            (stop-when game-over? final-render))) 
 
(tetris-main 1/4)