;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define handin "a10")
(define collaboration-statement "I worked alone")
(require 2htdp/image)

;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;

; join-together : [ListOf X] [ListOf Y] -> [ListOf XY]
; (join-together ls1 ls2) : returns the list with the top-level
; elements of ls1 followed by ls2
(define (join-together ls1 ls2)
  (cond
    [(empty? ls1) ls2]
    [else (local [ ; help : [ListOf X] -> [ListOf Y]
                  (define (help ls1)
                  (cons (first ls1) (join-together (rest ls1) ls2)))]
            (help ls1))]))
(check-expect (join-together '(a b c) '(d e f g h))
              (list 'a 'b 'c 'd 'e 'f 'g 'h))
(check-expect (join-together '() '(7 2 0 1 8 3 4))
              (list 7 2 0 1 8 3 4))
(check-expect (join-together '() '()) '())
(check-expect (join-together '(a b c) '())
              (list 'a 'b 'c)) 

;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;

;;;;; Part a

; flatten/v1 : [ListOf X] -> [ListOf X]
; (flatten/v1 lls) returns the result of
; combining all lists within lls into one
; list
(define (flatten/v1 lls)
  (cond
    [(empty? lls) '()]
    [else (join-together (first lls)
                         (flatten/v1 (rest lls)))]))
(check-expect (flatten/v1 '()) '())
(check-expect (flatten/v1'((a a a a) (b b) (c c c c c c) () (d) (e e e)
                                     ))(list 'a 'a 'a 'a 'b 'b 'c 'c
                                             'c 'c 'c 'c 'd 'e 'e 'e))
(check-expect (flatten/v1 '((b b) (a a) (c c)))
              (list 'b 'b 'a 'a 'c 'c))

; flatten/v2 : [ListOf X] -> [ListOf X]
; (flatten/v2 lls) returns the result of
; combining all lists within lls into one
; list with two base cases.
(define ls1 (make-list 50 (make-list 50 'a)))
(define (flatten/v2 lls)
  (cond
    [(empty? lls) '()]
    [(empty? (rest lls)) (first lls)]
    [else (flatten/v2 (cons (join-together (first lls) (second lls))
                            (rest (rest lls))))]))
(check-expect (flatten/v2 '()) '())
(check-expect (flatten/v2'((a a a a) (b b) (c c c c c c) () (d) (e e e)
                                     ))(list 'a 'a 'a 'a 'b 'b 'c 'c
                                             'c 'c 'c 'c 'd 'e 'e 'e))
(check-expect (flatten/v2 '((b b) (a a) (c c)))
              (list 'b 'b 'a 'a 'c 'c))

;;;;; Part b

#|

(flatten/v1 '((a b c) (d e) (f g h i)))
== (join-together '(a b c) (flatten/v1 '((d e) (f g h i))))
== (flatten/v1 '((d e) (f g h i)))
== (join-together '(a b c d e) (flatten/v1 '(f g h i)))
== (flatten/v1 '(f g h i))
== (join-together '(a b c d e f g h i) (flatten/v1 '()))
== (flatten/v1 '())
== (list 'a 'b 'c 'd 'e 'f 'g 'h 'i)

|#

;;;;; Part c

#|

(flatten/v2 '((a b c) (d e) (f g h i)))
== (flatten/v2 (cons (join-together '(a b c) '(d e)) '((f g h i))))
== (flatten/v2 '((a b c d e) (f g h i)))
== (flatten/v2 (cons (join-together '(a b c d e) '(f g h i)) '()))
== (flatten/v2 '(a b c d e f g h i)))
== (list 'a 'b 'c 'd 'e 'f 'g 'h 'i)

|#

;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;

;;;;; Part a

#|
Windows
Size of Compiler : 32

n  |  flatten/v1  | flatten/v2
===============================
20  cpu time : 0  cpu time : 0
50  cpu time : 0  cpu time : 63
100 cpu time : 0  cpu time : 485
200 cpu time : 47 cpu time : 6984
1000 cput time : 0 cpu time : 1500 


|#

;;;;; Part b

(define flatten flatten/v1)

;;;;; Part c

; The function known as flatten/v2 compared to flatten/v1
; takes a considerably longer time when attempting to test for
; longer lists in the repl. This is due to the inefficient method
; of recursing information and calling the function over and over again.
; The built-in cons function builds a list for a given set of values and
; ends with an empty list. The flatten/v2 function requires that a cons
; function always be made while recursing each given list until it
; reaches the empty list, and ends the function. It also requires two
; base cases for the function to go through, which means more time spent
; going through the test. While the other function flatten/v1 only
; requires one base case to check if the list gets to empty, and calls
; the join-together function without needing to use cons.

;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;

; list-head : Nat [ListOf Nat] -> [ListOf Nat]
; (list-head n lnat) : returns the list with the
; first n elements of that given list.
(define (list-head init-n init-ls)
  (local ; help : Nat [ListOf Nat] -> [ListOf Nat]
    [(define (help n ls)
       (cond
         [(zero? n) '()]
         [(empty? ls)
          (error 'list-head
                 (format "~s is too large for ~s"
                         init-n init-ls))] 
         [else
          (cons (first ls)
                (help (sub1 n) (rest ls)))]))]
    (help init-n init-ls)))
(check-error (list-head 5 '(a b c))
             "list-head: 5 is too large for (a b c)")
(check-expect (list-head 3 '(a b c)) (list 'a 'b 'c))
(check-expect (list-head 5 '(a b c d e)) (list 'a 'b 'c 'd 'e))
(check-expect (list-head 0 '()) '())
(check-expect (list-head 3 '(a b c d e)) (list 'a 'b 'c))

; list-tail : Nat [ListOf Nat] -> [ListOf Nat]
; (list-head n lnat) : returns the list with
; everything but the first n elements of the given
; list
(define (list-tail init-n init-ls)
  (local ; help : Nat [ListOf Nat] -> [ListOf Nat]
    [(define (help n ls)
       (cond
         [(zero? n) ls]
         [(empty? ls)
          (error 'list-tail
                 (format "~s is too large for ~s"
                         init-n init-ls))]
         [else (help (sub1 n)(rest ls))]))]
    (help init-n init-ls)))  
(check-error (list-tail 5 '(a b c))
             "list-tail: 5 is too large for (a b c)")
(check-expect (list-tail 3 '(a b c d e f)) '(d e f))
(check-expect (list-tail 0 '(a b c)) '(a b c))
(check-expect (list-tail 1 '(a b c)) '(b c))

;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;

; pop-up : PosInt [ListOf Y] -> [ListOf Y]
; (pop-up n ls) : returns the result of
; grouping runs of n elements in ls into
; separate sublists.
(define (pop-up n ls)
  (cond
    [(empty? ls) '()] 
    [else (cons (list-head n ls) (pop-up n (list-tail n ls)))]))
(check-expect (pop-up 2 '(a b c d e f g h)) (list (list 'a 'b)
                                                  (list 'c 'd)
                                                  (list 'e 'f)
                                                  (list 'g 'h)))
(check-expect (pop-up 3 '(a b c d e f)) (list (list 'a 'b 'c)
                                              (list 'd 'e 'f)))
(check-expect (pop-up 5 (make-list 20 0)) (list (list 0 0 0 0 0)
                                                (list 0 0 0 0 0)
                                                (list 0 0 0 0 0)
                                                (list 0 0 0 0 0)))
(check-expect (pop-up 0 '()) '())

;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;

; overwrite/slow : [ListOf X] Nat X -> [ListOf X]
; (overwrite/slow ls i x) returns the list that results from
; replacing the element at index i in ls with x.
 
(define (overwrite/slow ls i x)
  (join-together (list-head i ls)
    (join-together (list x) (list-tail (add1 i) ls))))
(check-expect (overwrite/slow '(a) 0 'b) '(b))
(check-expect (overwrite/slow '(a b c) 0 '_) '(_ b c))
(check-expect (overwrite/slow '(a b c) 1 '_) '(a _ c)) 
(check-expect (overwrite/slow '(a b c) 2 '_) '(a b _))
(check-expect (overwrite/slow '(x x x _ x x x x) 3 'x) (make-list 8 'x))

;;;;; Part a

; overwrite : [ListOf X] Nat X -> [ListOf X]
; (overwrite ls i x) returns the list that results
; from replacing the element at index i in ls with x
(define (overwrite ls i x)
  (local ; help : [ListOf X] Nat X -> [ListOf X]
    [ (define (help nls ni nx)
        (cond
          [(empty? nls)
           (error 'overwrite
                 (format "~s is out of bounds for ~s"
                         i ls))]
          [(zero? ni) (cons nx (rest nls))] 
          [else (cons (first nls) (help (rest nls) (sub1 ni) nx))]))]  
    (help ls i x))) 
(check-error (overwrite '(5 4 3 2 1) 5 6)
             "overwrite: 5 is out of bounds for (5 4 3 2 1)")
(check-error (overwrite '(5 4 3 2 1) -3 6)
             "overwrite: -3 is out of bounds for (5 4 3 2 1)")
(check-expect (overwrite '(a) 0 'b) '(b))
(check-expect (overwrite '(a b c) 0 '_) '(_ b c))
(check-expect (overwrite '(a b c) 1 '_) '(a _ c)) 
(check-expect (overwrite '(a b c) 2 '_) '(a b _))
(check-expect (overwrite '(x x x _ x x x x) 3 'x) (make-list 8 'x))

;;;;; Part b

#|

n     |  overwrite  | overwrite/slow
==========================================
10000 time : 0       31
      time : 0       31
      time : 15      32
      time : 0       31
      time : 0       31
|#

#|  === Summary ===
   The newly constructed overwrite function is designed as a one pass
   recursive solution to overwriting an elemnt in a list. Thus, it is
   considerably faster as it goes through less testing cases and
   functions to pass. The overwrite/slow function is SLOWER, as it goes
   through several helper functions such as join-together to pass, but
   is not as slow in comparison to the new overwrite function.
|#

;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;

;;;;; Part a

#|
   TileValue is one of :
    - 2
    - 4
    - 6
    - 8
    - 16
    - 32
    - 64
    - 128
    - 256
    - 512
    - 1024
    - 2048
|#

(define BLANK '_)

; blank? : X -> Bool
; (blank? item) : returns #true if the
; given variable item corresponds with the
; BLANK tile.
(define (blank? item)
  (if (equal? item BLANK)
      #true
      #false))
(check-expect (blank? BLANK) #true)
(check-expect (blank? 2048) #false)
(check-expect (blank? blank?) #false)

;;;;; Part b

#|
   A Board is one of
   - '()
   - (List [ListOf X] [ListOf X])

|# 

(define b1 (list (list 64 32)
       (list 16 16)))

(define b2 (list (list 2 2 2 2)
       (list 4 '_ 4 '_)
       (list '_ 8 8 8)
       (list 16 '_ '_ 16)))

(define b3 (list (list 16 64 8 256 4)
       (list 1024 1024 1024 32 128)
       (list 64 32 128 '_ '_)
       (list 4 4 32 '_ '_)
       (list 2 '_ '_ 512 '_)))

;;;;; Part c

; board-full? : Board -> Bool
; (board-full? board) : returns #true
; if the board has no blank tiles.
(define (board-full? board)
 (empty?(filter blank? (flatten board))))
(check-expect (board-full? '((4))) #true)
(check-expect (board-full? b1) #true)
(check-expect (board-full? b2) #false)
(check-expect (board-full? b3) #false)

;;;;; Part d

; random24 : Num -> NumF
; (random24 n) : returns either a
; 2 or 4 at random, with the value 2
; selected at 80% chance.
(define (random24 n)
  (cond
    [(equal? n 0) 2]
    [(equal? n 1) 2]
    [(equal? n 2) 2]
    [(equal? n 3) 2]
    [(equal? n 4) 4]
    [else #false]))
(check-expect (random24 0) 2)
(check-expect (random24 4) 4)
(check-expect (random24 3) 2)

; add-new-tile : Board -> Board
; (add-new-tile board) : returns a Board
; with a newly replaced TileValue of either
; 2 or 4 where the 2 is chosen 80% of the time.
; A blank is selected at random, and if the board
; is full, it is returned with no change.
(define (add-new-tile board)
  (local [(define flat (flatten board))]
  (local [(define randomindex (random (length flat)))]
  (cond
    [(board-full? board) board]
    [(blank? (list-ref flat randomindex))
     (pop-up (length (first board))
             (overwrite flat randomindex (random24 (random 5))))] 
    [else (add-new-tile board)]))))
(check-expect (add-new-tile '((4))) (list (list 4)))
(check-expect (add-new-tile b1) (list (list 64 32) (list 16 16)))

;;;;; Part e

; iterate : [Any -> Any] Nat Any -> Any
; (iterate f n x) : returns the result of
; using funct an n times starting with x
(define (iterate f n x)
  (cond
    [(zero? n) x]
    [else (iterate f (sub1 n) (f x))]))
(check-expect (iterate add1 4 3) 7)
(check-expect (iterate sub1 100 3) -97)

; This definition is tail-recursive because
; the x variable can change throughout the
; recursion.

; make-board ; PosInt Nat -> Board
; (make-board n m) : returns an nxn
; board with m non-blank tiles 
(define (make-board n m)
  (iterate add-new-tile m (make-list n (make-list n BLANK))))
(check-expect (make-board 1 0)
              (list (list '_)))
(check-expect (make-board 1 1)
              (list (list 2)))

;;;;; Part f

; board-square? Board -> Bool
; (board-square? board) : returns #true
; if the given Board is of dimension nxn
; for some positive integer n.
(define (board-square? board)
  (local ; Board -> Bool
    [(define (helper ltile answ)
          (cond 
            [(equal? (length board) (length ltile)) answ]
            [else #false]))]
  (foldr helper (or #true #false) board)))
(check-expect (board-square? b3) #true)
(check-expect (board-square? (make-board 123 0)) #true)
(check-expect (board-square? '((2 4) (8 16 32))) #false)
(check-expect (board-square? '((2 2 2 2) (4 4 4 4) (2 2 2) (4 4 4 4)))
              #false) 

;;;;; Part g

; game-won? : Board -> Bool
; (game-won? board) : returns #true
; if the board contains a 2048 tile.
(define (game-won? board)
  (member? 2048 (flatten board)))
(check-expect (game-won? b1) #false)
(check-expect (game-won? b2) #false)
(check-expect (game-won? b3) #false)
(check-expect (game-won? (list (list 2 2 2) (list 2 2048 2)
                               (list BLANK BLANK BLANK))) #true)

;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;

;;;;; Part a

(define TILE-SIZE 90)
(define FONT-SIZE 45)
(define GRID-SPACING 8)
(define GRID-COLOR (make-color 204 192 179))

;;;;; Part b

; tile->image : TileValue FONT-SIZE GRID-COLOR GRID-COLOR -> Image
(define (tile->image tile f backc frontc)
  (overlay/align "center" "center"
                 (if (blank? tile)
                     empty-image
                     (text (number->string tile) f backc))
                 (square TILE-SIZE "solid" frontc)
                 (square (+ GRID-SPACING TILE-SIZE) "solid"
                         GRID-COLOR)))
(check-satisfied (tile->image 64 FONT-SIZE
    (make-color 255 255 255) (make-color 246 94 59)) image?)
(check-expect (image-height(tile->image 64 FONT-SIZE
    (make-color 255 255 255) (make-color 246 94 59))) 98)
(check-expect (image-width(tile->image 64 FONT-SIZE
    (make-color 255 255 255) (make-color 246 94 59))) 98)

;;;;; Part c

; val->image : TileValue -> Image
; (val->image tile) : returns the image
; according to the game sizes and colors.
(define (val->image tile)
  (cond
    [(blank? tile) (tile->image tile FONT-SIZE (make-color 204 192 179)
                             (make-color 105 105 105))] 
    [(= tile 2) (tile->image 2 FONT-SIZE (make-color 105 105 105)
                             (make-color 238 228 218))]
    [(= tile 4) (tile->image 4 FONT-SIZE (make-color 105 105 105)
                             (make-color 237 224 200))]
    [(= tile 8) (tile->image 8 FONT-SIZE (make-color 255 255 255)
                             (make-color 242 177 121))]
    [(= tile 16) (tile->image 16 FONT-SIZE (make-color 255 255 255)
                             (make-color 245 149 99))]
    [(= tile 32) (tile->image 32 FONT-SIZE (make-color 255 255 255)
                             (make-color 246 124 95))]
    [(= tile 64) (tile->image 64 FONT-SIZE (make-color 255 255 255)
                             (make-color 246 94 59))]
    [(= tile 128) (tile->image 128 FONT-SIZE (make-color 255 255 255)
                             (make-color 237 207 114))]
    [(= tile 256) (tile->image 256 FONT-SIZE (make-color 255 255 255)
                             (make-color 237 200 80))]
    [(= tile 512) (tile->image 512 FONT-SIZE (make-color 255 255 255)
                             (make-color 237 200 80))]
    [(= tile 1024) (tile->image 1024 FONT-SIZE (make-color 255 255 255)
                             (make-color 237 197 63))]
    [(= tile 2048)  (tile->image 2048 FONT-SIZE (make-color 255 255 255)
                             (make-color 237 194 46))]))
(check-expect (image-height(val->image 2)) 98)
(check-expect (image-height(val->image 4)) 98)
(check-expect (image-width(val->image 64)) 98) 


;;;;; Part d

; board->image : Board -> Image
; (board->image board) : returns the
; corresponding image of a board
;(define (board->image board)
;  (local [(define (next a)
;            (cond
;              [(empty? a) empty-image]
;              [else (beside (first a) (next (rest a)))]))
;          (define (a-list b)
;            (cond
;              [(empty? b) '()]
;              [else (cons (next (first b)) (a-list (rest b)))]))]))






  
      
  
















#|

(foldr above empty-image
(map (lambda (l)
     (foldr beside empty-image l))
(pop-up 4 (maptile->image (flatten ls))))

|#
  


    









