#lang racket
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(check-location "09" "q1.rkt")


(provide
 World<%>
 Widget<%>
 Metatoy<%>
 Toy<%>
 run
 make-metatoy
 make-throbber
 make-clock
 make-politician)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;CONSTANT-DEFINITIONS:

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-XCENTER (/ CANVAS-WIDTH 2))
(define CANVAS-YCENTER (/ CANVAS-HEIGHT 2))

(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLTICIAN-EVENT "p")

(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define MOVE "move")

(define THROBBER-RADIUS 5)
(define SOLID-VARIABLE "solid")
(define OUTLINE-VARIABLE "outline")
(define GREEN-COLOR "green")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACES :

;; A World is an object of any class that implements World<%> 
;; big-bang will communicate with the world through the World<%>
;; interface. 

(define World<%>
  (interface ()
    
    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          
    
    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event
    
    ; KeyEvent : KeyEvent -> World
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     
    
    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Every object that lives in the world must implement the Widget<%>
;; interface.

(define Widget<%>
  (interface ()
    
    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          
    
    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     
    
    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Metatoy is an object of any class that implements Metatoy<%>.


(define Metatoy<%>
  (interface (World<%>)
    ;; this means: include all the methods in World<%>
    ;; -> ListOfToy
    get-toys
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Toy is an object of any class that implements Toy<%>
;; three such classes, one for each kind of toy 

(define Toy<%> 
  (interface (Widget<%>)  ;; this means: include all the methods in
    ;;  Widget<%>.
    
    
    
    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move
    
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA-DEFINITONS:

;;A ListOfToy(LOT) is one of the following:
;; --empty
;; --(cons T LOT)

;;TEMPLATE:
;;lot-fn : LOT -> ?
#|
(define (lot-fn lot)
    (cond
      [(empty? lot)...]
      [else
       (...(first lot))
        (lot-fn (rest lot)))]))
|#


;;A ListOfWidget(LOW) is one of the following:
;; --empty
;; --(cons W LOW)

;;TEMPLATE:
;;low-fn : LOW -> ?
#|
(define (low-fn low)
    (cond
      [(empty? low)...]
      [else
       (...(first low))
        (low-fn (rest low)))]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION-DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum -> Metatoy
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates a MetaToy with no toys in it, and runs it using big-bang
;; at the given frame rate.
;; RETURNS :Returns the final state of the Metatoy.

(define (run rate)
  (big-bang
   (initial-metatoy)
   (on-tick
    (lambda (metatoy) (send metatoy after-tick))
    rate)
   (on-draw
    (lambda (metatoy) (send metatoy to-scene)))
   (on-key
    (lambda (metatoy kev)(send metatoy after-key-event kev)))
   (on-mouse
    (lambda (metatoy mx my mev) (send metatoy after-mouse-event mx my mev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Metatoy% class

;; Constructor template for Metatoy%:
;; A Metatoy is a 
;; (new Metatoy% [toys ListOfToys])
;; Interpretation: An object of class Metatoy% takes signals from
;; big-bang and distributes them to its objects as appropriate.

;; make-metatoy : ListOfToys -> Metatoy
;; GIVEN: a list of toys
;; RETURNS: an object of class Metatoy% containing the given list of toys
(define (make-metatoy toys)
  (new Metatoy% [toys toys]))

;; initial-metatoy : -> Empty list of Metatoy
;; RETURNS: an object of class Metatoy% containing the no toys

(define (initial-metatoy)
  (make-metatoy empty))


;;TESTS :
(begin-for-test
  (check-equal?
   (equal?
    (initial-metatoy)
    (make-metatoy empty)) false))


(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field toys) ;  ListOfToys
    
    
    (super-new)
    
    ;; after-tick : -> World
    ;; STRATEGY : Use HOFC map on the Widget's in this World
    
    (define/public (after-tick)
      (make-metatoy
       (map
        (lambda (toy) (send toy after-tick))
        toys)))
    
    
    ;; to-scene : -> Scene
    ;; STRATEGY : Use HOFC foldr on the Widgets in this World
    
    (define/public (to-scene)
      (foldr
       (lambda (obj scene)
         (send obj add-to-scene scene))
       EMPTY-CANVAS
       toys)) 
    
    
    ;; after-key-event : KeyEvent -> World
    ;; STRATEGY: Cases on kev
    ;; "t" creates a throbber mini-toy
    ;; "c" creates clock mini-toy
    ;; "p" politician mini-toy
    ;; other keystrokes are passed on to the objects in the world
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (make-metatoy
          (cons (make-throbber CANVAS-XCENTER CANVAS-YCENTER) toys))]
        
        [(key=? kev NEW-CLOCK-EVENT)
         (make-metatoy
          (cons (make-clock CANVAS-XCENTER CANVAS-YCENTER) toys))]
        
        [(key=? kev NEW-POLTICIAN-EVENT)
         (make-metatoy
          (cons (make-politician CANVAS-XCENTER CANVAS-YCENTER) toys))]
        
        [else
         (make-metatoy
          (map
           (lambda (toy) (send toy after-key-event kev))
           toys))]))
    
    
    ;; metatoy-after-mouse-event : Nat Nat MouseEvent -> World
    ;; STRATEGY: Cases on mouse event mev
    
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev BUTTON-DOWN)
         (world-after-button-down mx my)]
        [(mouse=? mev DRAG)
         (world-after-drag mx my)]
        [(mouse=? mev BUTTON-UP)
         (world-after-button-up mx my)]
        [(mouse=? mev MOVE)
         (world-after-move mx my)]
        [else this]))
    
    ;; the next few functions are local functions, not in the interface.
    
    
    ;;world-after-button-down : Int Int -> world
    ;;GIVEN: mouse positions
    ;;RETURNS: toy state
    ;;STRATEGY: Using HOF map on toy
    
    (define (world-after-button-down mx my)
      (make-metatoy
       (map
        (lambda (toy) (send toy after-button-down mx my))
        toys)))
    
    
    ;;world-after-button-up : Int Int -> world
    ;;GIVEN: mouse positions
    ;;RETURNS: toy state
    ;;STRATEGY: Using HOF map on toy
    
    (define (world-after-button-up mx my)
      (make-metatoy
       (map
        (lambda (toy) (send toy after-button-up mx my))
        toys)))
    
    ;;world-after-button-drag : Int Int -> world
    ;;GIVEN: mouse positions
    ;;RETURNS: toy state
    ;;STRATEGY: Using HOF map on toy
    
    (define (world-after-drag mx my)
      (make-metatoy
       (map
        (lambda (toy) (send toy after-drag mx my))
        toys)))
    
    ;;world-after-button-drag : Int Int -> world
    ;;GIVEN: mouse positions
    ;;RETURNS: toy state
    ;;STRATEGY: Using HOF map on toy
    
    (define (world-after-move mx my)
      (make-metatoy
       (map
        (lambda (toy) (send toy after-move mx my))
        toys)))
    
    
    
    ;; -> ListOfToy
    ;;GIVEN :
    ;;RETURNS : list of toys
    
    (define/public (get-toys)
      toys)))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have three classes for Toy<%> : Throbber, Clock and Politician
;; Throbber appears at the center of canvas and increase and reduces in size

;; Constructor template of Throbber%:
;; A Throbber is a 
;; (new Throbber% [throbber-x Int][throbber-y Int]
;;                [thobber-radius NonNegInt]
;;                [mouse-xcoordinate Int] [mouse-ycoordinate Int]
;;                [throbber-selected? Boolean]
;;                [thober-radius-factor Int]
;;                [throbber-mode String]
;;                

;; INTERPRETATION:
;; Represents a throbber toy in the world with initial radius of 5
;; which expands to 20 and reduces back to 5.
;; throbber-x is the x-coordinate of the center of throbber 
;; throbber-y is the y-coordinate of the center of throbber
;; mouse-xcoordinate is the x-coordinate of mouse point if throbber is selected
;; mouse-ycoordinate is the y-coordinate of mouse point if throbber is selected
;; selected? is true throbber is selected, false otherwise
;; throbber-radius is the current radius of the throbber
;; throbber-radius-factor represents if radius is expanding or reducing
;; throbber-mode represents the outline or solid for the throbbers

(define Throbber% 
  (class* object% (Toy<%>)
    
    (init-field
     throbber-x
     throbber-y) ;;Initialising attributes for throbber's x and y coordinates
    
    (init-field
     [mouse-xcoordinate -1]
     [mouse-ycoordinate -1]) ;;initialising mouse x and y coordinates as -1
    
    (init-field
     [selected? false]) ;; initialising start throbber to be false
    
    (init-field
     [throbber-radius 5]) ;; initialising throbber start radius to be 5
    
    (init-field
     [throbber-radius-factor 2]) ;; initialising throbber radius factor to be 2
    
    (init-field
     [throbber-mode "solid"]) ;; initialising starting throbber to be solid
    
    (field [MIN-RADIUS 5])
    (field [MAX-RADIUS 20]) ;; initialising min and max radius

    
    (define THROBBER-IMAGE (circle throbber-radius throbber-mode GREEN-COLOR)) ;; defining a constant for throbber image
    
    (super-new)
    
    ;;  after-move: Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    
    (define/public (after-move mx my)
      this)
    
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS : x or y coordinate of center of throbber
    
    (define/public (toy-x) throbber-x)
    (define/public (toy-y) throbber-y)
    
    ;; toy-data : -> PosInt
    ;; RETURNS : radius of the throbber
    
    (define/public (toy-data) throbber-radius)
    
    ;; inside-throbber? : Integer Integer -> Boolean
    ;; GIVEN : a location on the canvas
    ;; RETURNS : true iff the location is inside this throbber.
    ;; STARTEGY : Combining simple function
    
    (define (inside-throbber? mx my)
      (<= (+ (sqr (- throbber-x  mx)) (sqr (- throbber-y my)))
          (sqr throbber-radius)))
    
    
    ;;after-tick : -> Toy<%>
    ;;RETURNS : A Toy<%>  with expanding or reducing radius
    
    (define/public (after-tick)
      (cond
        [(equal? selected? true) this]
        [(>= (+ throbber-radius throbber-radius-factor) MAX-RADIUS)
         (new Throbber%
              [throbber-x throbber-x]
              [throbber-y throbber-y]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [selected? selected?]
              [throbber-radius MAX-RADIUS]
              [throbber-radius-factor (- throbber-radius-factor)]
              [throbber-mode throbber-mode])]
        [(< (+ throbber-radius throbber-radius-factor) MIN-RADIUS)
         (new Throbber%
              [throbber-x throbber-x]
              [throbber-y throbber-y]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [selected? selected?]
              [throbber-radius MIN-RADIUS]
              [throbber-radius-factor (- throbber-radius-factor)]
              [throbber-mode throbber-mode])]
        [else
         (new Throbber%
              [throbber-x throbber-x]
              [throbber-y throbber-y]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [selected? selected?]
              [throbber-radius (+ throbber-radius throbber-radius-factor)]
              [throbber-radius-factor throbber-radius-factor]
              [throbber-mode throbber-mode])]))
    
    
    ;; after-key-event : KeyEvent -> Toy
    ;; GIVEN : key event "t"
    ;; RETURNS : throbber mini-toy
    
    (define/public (after-key-event kev)
      this)
    
    ;; after-button-down :  Integer Integer -> Toy
    ;; GIVEN: the location of a button-down event
    ;; STRATEGY: Cases on whether the event is in the throbber
    
    (define/public (after-button-down mx my)
      (if (inside-throbber? mx my)
          (new Throbber%
               [throbber-x throbber-x]
               [throbber-y throbber-y]
               [mouse-xcoordinate mx]
               [mouse-ycoordinate my]
               [selected? true]
               [throbber-radius throbber-radius]
               [throbber-radius-factor throbber-radius-factor]
               [throbber-mode "outline"])
          this))
    
    
    
    ;; after-button-up :  Integer Integer -> Toy
    ;; GIVEN: the location of a button-up event
    ;; STRATEGY: Cases on whether the event is in the throbber and if the
    ;; throbber is selected then unselected it.
    
    (define/public (after-button-up mx my)
      (if (inside-throbber? mx my)
          (new Throbber%
               [throbber-x throbber-x]
               [throbber-y throbber-y]
               [mouse-xcoordinate 0]
               [mouse-ycoordinate 0]
               [selected? false]
               [throbber-radius throbber-radius]
               [throbber-radius-factor throbber-radius-factor]
               [throbber-mode "solid"])
          this))
    
    ;; after-drag : Integer Integer -> Toy
    ;; GIVEN: the location of a drag event
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [throbber-x (- mx (- mouse-xcoordinate throbber-x))]
               [throbber-y (- my (- mouse-ycoordinate throbber-y))]
               [mouse-xcoordinate mx]
               [mouse-ycoordinate my]
               [selected? selected?]
               [throbber-radius throbber-radius]
               [throbber-radius-factor throbber-radius-factor]
               [throbber-mode throbber-mode])
          this))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this
    ;; throbber painted on it.
    
    (define/public (add-to-scene scene)
      (place-image THROBBER-IMAGE throbber-x throbber-y scene))))




;; for-tests
#; (define/public (for-test:toy-selected?) selected?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clock appears at the center of canvas and clock tick is displayed inside it.


;; Constructor template of Clock%:
;; A Clock is a 
;; (new Clock% [clock-x Int] [clock-y Int]
;;   [mouse-xcoordinate Int] [mouse-ycoordinate Int]
;;                [clock-selected? Boolean]
;;                [clock-counter 0])


;; INTERPRETATION:
;; Represents a clock toy in the world 
;; clock-x is the x-coordinate of the center of clock 
;; clock-y is the y-coordinate of the center of clock
;; mouse-xcoordinate is the x-coordinate of mouse point if clock is selected
;; mouse-ycoordinate is the y-coordinate of mouse point if clock is selected
;; selected? is true clock is selected
;; clock-counter is the number of ticks since it was created

(define Clock% 
  (class* object% (Toy<%>)
    
    (init-field
     clock-x
     clock-y) ;; initialising clock x and y coordinates
    
    (init-field
     [mouse-xcoordinate -1]
     [mouse-ycoordinate -1]) ;;initialising mouse coordinates
    
    (init-field
     [selected? false]) ;; initialising initial clock to be unselected
    
    (init-field
     [clock-counter 0]) ;; initialising clock-counter to be 0
    
    (field [SIDE-LENGTH 80])
    (field [SIDE-HEIGHT 30])
    (field [CLOCK-INITIAL-IMAGE (rectangle SIDE-LENGTH SIDE-HEIGHT "outline" "blue")])
    ;;initialising length and height of the rectangle and an initial image 
    
    (super-new)
    
    ;;  after-move: Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    
    (define/public (after-move mx my)
      this)
    
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS : x or y coordinate of center of clock
    
    (define/public (toy-x) clock-x)
    (define/public (toy-y) clock-y)
    
    ;; toy-data : -> PosInt
    ;; RETURNS : increasing clock ticks with display inside clock
    
    (define/public (toy-data) clock-counter)

    
    
    (define CLOCK-IMAGE (overlay (text (number->string clock-counter) 12 "red")
                                 CLOCK-INITIAL-IMAGE)) ;; CONSTANT for clock image
    
    
    
    ;; inside-clock? : Integer Integer -> Boolean
    ;; GIVEN : a location on the canvas
    ;; RETURNS : true iff the location of the mouse coordinates is inside this clock
    
    (define (inside-clock? mx my)
      (and
       (and (>= mx (- clock-x (/ SIDE-LENGTH 2)))
            (<= mx (+ clock-x (/ SIDE-LENGTH 2)))
            (and (>= my (- clock-y (/ SIDE-HEIGHT 2)))
                 (<= my (+ clock-y (/ SIDE-HEIGHT 2)))))))
    
    ;;after-tick : -> Toy<%>
    ;;RETURNS : A Toy<%>  with increasing clock ticks
    
    (define/public (after-tick)
      (new Clock%
           [clock-x clock-x]
           [clock-y clock-y]
           [mouse-xcoordinate mouse-xcoordinate]
           [mouse-ycoordinate mouse-ycoordinate]
           [selected? selected?]
           [clock-counter (+ 1 clock-counter)]))
    
    
    
    ;; after-key-event : KeyEvent -> Toy
    ;; GIVEN : key event "c"
    ;; RETURNS : clock mini-toy
    
    (define/public (after-key-event kev)
      this)
    
    ;; after-button-down :  Integer Integer -> Toy
    ;; GIVEN: the location of a button-down event
    ;; STRATEGY: Cases on whether the event is in the clock
    
    (define/public (after-button-down mx my)
      (if (inside-clock? mx my)
          (new Clock%
               [clock-x clock-x]
               [clock-y clock-y]
               [mouse-xcoordinate mx]
               [mouse-ycoordinate my]
               [selected? true]
               [clock-counter clock-counter])
          this))
    
    
    
    ;; after-button-up :  Integer Integer -> Toy
    ;; GIVEN: the location of a button-up event
    ;; STRATEGY: Cases on whether the event is in the clock and if the
    ;; clock is selected, then unselect it
    
    (define/public (after-button-up mx my)
      (if selected?
          (new Clock%
               [clock-x clock-x]
               [clock-y clock-y]
               [mouse-xcoordinate -1]
               [mouse-ycoordinate -1]
               [selected? false]
               [clock-counter clock-counter])
          this))
    
    ;; after-drag : Integer Integer -> Toy
    ;; GIVEN: the location of a drag event
    ;; STRATEGY: Cases on whether the clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [clock-x (+ clock-x (- mx mouse-xcoordinate))]
               [clock-y (+ clock-y (- my mouse-ycoordinate))]
               [mouse-xcoordinate mx]
               [mouse-ycoordinate my]
               [selected? selected?]
               [clock-counter clock-counter])
          this))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this
    ;; clock painted
    ;; on it
    
    (define/public (add-to-scene scene)
      (place-image CLOCK-IMAGE clock-x clock-y scene))
    
    
    
    
    ;; for-tests
    (define/public (for-test:toy-selected?) selected?)))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Politician moves in a straight line either towards the mouse position or away from it.
;; However, he never reaches or passes the mouse position.


;; Constructor template of Poltician%:
;; A Politician is a 
;; (new Politician%
;;  [politician-x Int] [politician-y Int]
;;   [mouse-xcoordinate Int] [mouse-ycoordinate Int]
;;                [politician-speed Int])
;;                


;; INTERPRETATION:
;; Represents a politician toy in the world 
;; politician-x is the x-coordinate of the center of politician 
;; politician-y is the y-coordinate of the center of politician
;; mouse-xcoordinate is the x-coordinate of mouse point 
;; mouse-ycoordinate is the y-coordinate of mouse point 
;; politician-speed is the speed with which politician moves in a straight line

(define Politician% 
  (class* object% (Toy<%>)
    
    (init-field
     politician-x
     politician-y) ;;Initialising attributes for politician x and y coordinates


    
    (init-field
     [mouse-xcoordinate -1]
     [mouse-ycoordinate -1]) ;; Initialising mouse coordinates to -1
    
    
    
    
    (init-field
     [politician-speed 110]) ;; Initialising politicial speed
    
    (init-field
     [hillary? true]) ;; Initialising with Hillary as true
    
    
    
    
    (field [POLITICIAN-1 (bitmap "politician1.jpg")])  ;; Field location for Trump
    (field [POLITICIAN-2 (bitmap "politician2.jpg")]) ;; field location for Hillary
    
    
    
    
    (super-new)
    
    ;;  after-move: Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    
    (define/public (after-move cursor-x cursor-y)
      (new Politician%
           [politician-x  politician-x]
           [politician-y  politician-y]
           [mouse-xcoordinate cursor-x]
           [mouse-ycoordinate cursor-y]
           [politician-speed politician-speed]
           [hillary? hillary?]))
    
    
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS : x or y coordinate of center of politician
    
    (define/public (toy-x) politician-x) 
    (define/public (toy-y) politician-y)
    
    ;; toy-data : -> PosInt
    ;; RETURNS : current distance to mouse
    
    (define/public (toy-data) (fetch-distance))


    (define (fetch-distance)
      (exact-floor
       (sqrt (+ (sqr (- mouse-xcoordinate politician-x)) (sqr (- mouse-ycoordinate politician-y))))))
    ;; Constant to calculate distance between the mouse coordinates and the politician


    ;; CONSTANTS :
    
    (define POLITICIAN-IMAGE1 POLITICIAN-1)
    (define POLITICIAN-IMAGE2 POLITICIAN-2)
    
    
    
    ;; after-tick : -> Toy<%>
    ;; RETURNS : A Toy<%>  moving in a straight line
    ;; STRATEGY : Combine simpler functions
    
    (define/public (after-tick)
      (if
       (>= (fetch-distance) 75)
       (move-towards-mouse) (move-away-from-mouse)))
    
    
    ;; move-toward-mouse: -> Politician%
    ;; RETURNS : Politician toy
    ;; STRATEGY : Combining simple functions
    
    (define (move-towards-mouse)
      (if (equal? hillary? true) 
          (new Politician%
               [politician-x   (/ (+ politician-x mouse-xcoordinate) 2)]
               [politician-y   (/ (+ politician-y mouse-ycoordinate) 2)]
               [mouse-xcoordinate mouse-xcoordinate]
               [mouse-ycoordinate mouse-ycoordinate]
               [politician-speed politician-speed]
               [hillary? true])
          (new Politician%
               [politician-x   (/ (+ politician-x mouse-xcoordinate) 2)]
               [politician-y   (/ (+ politician-y mouse-ycoordinate) 2)]
               [mouse-xcoordinate mouse-xcoordinate]
               [mouse-ycoordinate mouse-ycoordinate]
               [politician-speed politician-speed]
               [hillary? false])))
    
    ;; move-away-from-mouse: -> Politician%
    ;; RETURNS : Politician toy
    ;; STRATEGY : Combining simple functions
    
    (define (move-away-from-mouse)
      (if (equal? hillary? true)
          (return-trump) (return-hillary)))
    
    ;; return-trump : -> Politician%
    ;; RETURNS : Politician toy
    ;; WHERE : The toy is Trump
    ;; STRATEGY : Dividing into cases for the mouse coordinates when the toy moves
    
    (define (return-trump)
      (cond
        [(and (> mouse-xcoordinate politician-x) (> mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x (* -1 politician-speed))] 
              [politician-y  (+ politician-y (* -1 politician-speed))]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed] 
              [hillary? false])]
        [(and (< mouse-xcoordinate politician-x) (< mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x politician-speed)]
              [politician-y  (+ politician-y politician-speed)]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? false])]
        [(and (< mouse-xcoordinate politician-x) (> mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x politician-speed)]
              [politician-y  (+ politician-y (* -1 politician-speed))]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? false])]
        [(and (> mouse-xcoordinate politician-x) (< mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x (* -1 politician-speed))]
              [politician-y  (+ politician-y politician-speed)]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? false])]))
    
    ;; return-hillary: -> Politician%
    ;; RETURNS : Politician toy
    ;; WHERE : the toy is Hillary
    ;; STRATEGY : Dividing into cases for the mouse coordinates when the toy moves
    
    (define (return-hillary)
      (cond
        [(and (> mouse-xcoordinate politician-x) (> mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x (* -1 politician-speed))]
              [politician-y  (+ politician-y (* -1 politician-speed))]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? true])]
        [(and (< mouse-xcoordinate politician-x) (< mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x politician-speed)]
              [politician-y  (+ politician-y politician-speed)]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? true])]
        [(and (< mouse-xcoordinate politician-x) (> mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x politician-speed)]
              [politician-y  (+ politician-y (* -1 politician-speed))]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? true])]
        [(and (> mouse-xcoordinate politician-x) (< mouse-ycoordinate politician-y))
         (new Politician%
              [politician-x  (+ politician-x (* -1 politician-speed))]
              [politician-y  (+ politician-y politician-speed)]
              [mouse-xcoordinate mouse-xcoordinate]
              [mouse-ycoordinate mouse-ycoordinate]
              [politician-speed politician-speed]
              [hillary? true])]))
    
    
    
    ;; after-key-event : KeyEvent -> Toy
    ;; GIVEN : key event "p"
    ;; RETURNS : poltician mini-toy
    
    (define/public (after-key-event kev)
      this)
    
    
    
    ;; after-button-down :  Integer Integer -> Toy
    ;; GIVEN: the location of a button-down event
    ;; STRATEGY: 
    (define/public (after-button-down mx my)
      this)
    
    
    
    
    
    ;; after-button-up :  Integer Integer -> Toy
    ;; GIVEN: the location of a button-up event
    ;; STRATEGY: 
    (define/public (after-button-up mx my)
      this)
    
    
    
    
    
    ;; after-drag : Integer Integer -> Toy
    ;; GIVEN: the location of a drag event
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      this)
    
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this
    ;; throbber painted
    ;; on it
    
    (define/public (add-to-scene scene)
      (place-image
       (if
        (equal? hillary? true) POLITICIAN-IMAGE1 
        POLITICIAN-IMAGE2 )
       politician-x politician-y scene))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make-throbber: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: a new object of class Throbber% at center of the screen

(define (make-throbber throbber-x throbber-y)
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clock : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.

(define (make-clock clock-x clock-y)
  (new Clock%
       [clock-x CANVAS-XCENTER]
       [clock-y CANVAS-YCENTER]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-politician : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a politician at the given position

(define (make-politician politician-x politician-y)
  (new Politician%
       [politician-x CANVAS-XCENTER]
       [politician-y CANVAS-YCENTER]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS :

;;; THROBBER :


(define THROBBER-TEST
  (make-throbber CANVAS-XCENTER CANVAS-YCENTER))

(define THROBBER-MAX-RADIUS 20)

(define SELECTED-THROBBER
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]
       [mouse-xcoordinate 0]
       [mouse-ycoordinate 0]
       [selected? true]
       [throbber-radius THROBBER-RADIUS]
       [throbber-radius-factor THROBBER-RADIUS]
       [throbber-mode "solid"]))


(define THROBBER-MAX-RADIUS-TEST
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]
       [mouse-xcoordinate -1]
       [mouse-ycoordinate -1]
       [selected? false]
       [throbber-radius THROBBER-MAX-RADIUS]
       [throbber-radius-factor THROBBER-RADIUS]
       [throbber-mode "solid"]))


(define THROBBER-MIN-RADIUS-TEST
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]
       [mouse-xcoordinate 0]
       [mouse-ycoordinate 0]
       [selected? false]
       [throbber-radius THROBBER-RADIUS]
       [throbber-radius-factor THROBBER-RADIUS]
       [throbber-mode "outline"]))

(define THROBBER-MODE "solid")
(define THROBBER-COLOR "green")


(define THROBBER-IMAGE-TEST (circle THROBBER-RADIUS THROBBER-MODE THROBBER-COLOR))

(define THROBBER-IMG
  (place-image 
   THROBBER-IMAGE-TEST
   CANVAS-XCENTER
   CANVAS-YCENTER
   EMPTY-CANVAS))

(define INITIAL-WORLD-THROBBER-SELECTED
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]
       [mouse-xcoordinate -1]
       [mouse-ycoordinate -1]
       [selected? true]
       [throbber-radius THROBBER-RADIUS]
       [throbber-radius-factor THROBBER-RADIUS]
       [throbber-mode "solid"]))

(define INITIAL-WORLD-THROBBER-UNSELECTED
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]
       [mouse-xcoordinate 0]
       [mouse-ycoordinate 0]
       [selected? false]
       [throbber-radius 20]
       [throbber-radius-factor THROBBER-RADIUS]
       [throbber-mode "solid"]))

(define THROBBER-1
  (new Throbber%
       [throbber-x CANVAS-XCENTER]
       [throbber-y CANVAS-YCENTER]
       [mouse-xcoordinate 0]
       [mouse-ycoordinate 0]
       [selected? false]
       [throbber-radius THROBBER-RADIUS]
       [throbber-radius-factor -1]
       [throbber-mode "solid"]))

(begin-for-test
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-SELECTED after-tick) toy-data) 5
   "Test of  throbber after-tick with max radius failed ")
  (check-equal? (send (send THROBBER-1 after-tick) toy-data)
                5 "Test for unselected throbber after tick failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-UNSELECTED after-tick) toy-data)
   20 "Unselected throbber test for radius failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-UNSELECTED after-drag 300 340) toy-data)
   20 "Unselected throbber after-drag test failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-UNSELECTED after-key-event "t") toy-data)
   20 "Unselected throbber after key event for throbber test failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-UNSELECTED after-button-down 400 400) toy-data)
   20 "Unselected throbber after button down test failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-UNSELECTED after-button-up 400 400) toy-data)
   20 "Unselected throbber after button up test failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-UNSELECTED after-button-up 260 310) toy-data)
   20 "Unselected throbber after button down test failed")
  (check-equal?
   (send (send INITIAL-WORLD-THROBBER-SELECTED after-move 260 350) toy-data)
   5 "Selected throbber after move test failed")
  (check-equal?
   (send (send SELECTED-THROBBER after-tick) toy-y)
   CANVAS-YCENTER "Test of selected throbber after-tick failed ")
  (check-equal?
   (send (send THROBBER-TEST after-tick) toy-y)
   CANVAS-YCENTER "Test of unselected throbber after-tick test failed ")
  (check-equal?
   (send (send THROBBER-MAX-RADIUS-TEST after-tick) toy-data)
   THROBBER-MAX-RADIUS "Test of  throbber after-tick with max radius failed ")
  (check-equal?
   (send (send THROBBER-MIN-RADIUS-TEST after-tick) toy-x)
   CANVAS-XCENTER "Test of  throbber after-tick with min radius failed ")
  (check-equal?
   (send
    (send THROBBER-TEST after-button-down CANVAS-XCENTER CANVAS-YCENTER) toy-x)
   CANVAS-XCENTER "Test of  throbber after-button-down failed ")
  (check-equal?
   (send THROBBER-TEST add-to-scene EMPTY-CANVAS)
   THROBBER-IMG "Test of  throbber add-to-scene failed ")
  (check-equal?
   (send
    (send SELECTED-THROBBER after-drag 100 200) toy-x)
   350 "Test of throbber after-drag failed ")
  (check-equal?
   (send
    (send SELECTED-THROBBER after-drag 100 200) toy-y)
   500 "Test of throbber after-drag failed "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOCK :

(define CLOCK-TEST
  (make-clock CANVAS-XCENTER CANVAS-YCENTER))

(define SIDEL 80)
(define SIDEH 30)

(define CLOCK-IMAGE-TEST (rectangle SIDEL SIDEH "outline" "blue"))

(define SELECTED-CLOCK
  (new Clock%
       [clock-x CANVAS-XCENTER]
       [clock-y CANVAS-YCENTER]
       [mouse-xcoordinate 0]
       [mouse-ycoordinate 0]
       [selected? true]
       [clock-counter 0]))

(define UNSELECTED-CLOCK
  (new Clock%
       [clock-x CANVAS-XCENTER]
       [clock-y CANVAS-YCENTER]
       [mouse-xcoordinate 0]
       [mouse-ycoordinate 0]
       [selected? false]
       [clock-counter 0]))


(define CLOCK-IMG
  (place-image 
   (text "0" 12 "red")
   CANVAS-XCENTER
   CANVAS-YCENTER
   (place-image
    CLOCK-IMAGE-TEST
    CANVAS-XCENTER
    CANVAS-YCENTER
    EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
   (send (send UNSELECTED-CLOCK after-move 400 450) toy-y)
   300)
  (check-equal?
   (send (send UNSELECTED-CLOCK after-key-event "c") toy-y)
   300)
  (check-equal?
   (send (send UNSELECTED-CLOCK after-button-down 400 400) toy-y)
   300)
  (check-equal? (send (send UNSELECTED-CLOCK after-button-up 400 400) toy-y)
                300)
  (check-equal?(send (send UNSELECTED-CLOCK after-drag 400 400) toy-x)
               250)
  (check-equal?(send (send SELECTED-CLOCK after-button-up 400 400) toy-x)
               250)
  (check-equal?
   (send (send SELECTED-CLOCK after-tick) toy-y)
   CANVAS-YCENTER "Test of selected clock after-tick failed")
  (check-equal?
   (send (send CLOCK-TEST after-tick) toy-y)
   CANVAS-YCENTER "Test of clock after-tick with y-coordinate failed")
  (check-equal?
   (send (send CLOCK-TEST after-tick) toy-x)
   CANVAS-XCENTER "Test of  clock after-tick with x-coordinate failed ")
  (check-equal?
   (send
    (send CLOCK-TEST after-button-down CANVAS-XCENTER CANVAS-YCENTER) toy-data)
   0 "Test of clock after-button-down with time value failed")
  (check-equal?
   (send CLOCK-TEST add-to-scene EMPTY-CANVAS)
   CLOCK-IMG "Test of clock add-to-scene failed ")
  (check-equal?
   (send
    (send SELECTED-CLOCK after-drag 400 350) toy-x)
   650 "Test of  clock after-drag failed ")
  (check-equal?
   (send
    (send SELECTED-CLOCK after-drag 400 150) toy-y)
   450) "Test of  clock after-drag failed ")

;; POLITICIAN :


(define POLITICIAN-TEST-CANVASCENTER
  (new Politician%
       [politician-x CANVAS-XCENTER]
       [politician-y CANVAS-YCENTER]
       [mouse-xcoordinate 326]
       [mouse-ycoordinate 376]
       [politician-speed 10]
       [hillary? true]))

(define POLITICIAN-TEST-CANVASCENTER-TRUMP
  (new Politician%
       [politician-x CANVAS-XCENTER]
       [politician-y CANVAS-YCENTER]
       [mouse-xcoordinate 326]
       [mouse-ycoordinate 376]
       [politician-speed 10]
       [hillary? false]))

(define POLITICIAN-TEST-TOWARDS-MOUSE
  (new Politician%
       [politician-x 320]
       [politician-y 370]
       [mouse-xcoordinate 300]
       [mouse-ycoordinate 350]
       [politician-speed 10]
       [hillary? true]))

(define POLITICIAN-TEST-2
  (new Politician%
       [politician-x 320]
       [politician-y 370]
       [mouse-xcoordinate 370]
       [mouse-ycoordinate 350]
       [politician-speed 10]
       [hillary? true]))

(define POLITICIAN-TEST-3
  (new Politician%
       [politician-x 400]
       [politician-y 300]
       [mouse-xcoordinate 410]
       [mouse-ycoordinate 250]
       [politician-speed 10]
       [hillary? false]))

(define POLITICIAN-MOUSE-XY-GREATER
  (new Politician%
       [politician-x 280]
       [politician-y 380]
       [mouse-xcoordinate 300]
       [mouse-ycoordinate 400]
       [politician-speed 110]
       [hillary? true]))
(define POLITICIAN-MOUSE-Y-GREATER
  (new Politician%
       [politician-x 280]
       [politician-y 380]
       [mouse-xcoordinate 250]
       [mouse-ycoordinate 400]
       [politician-speed 110]
       [hillary? true]))
(define POLITICIAN-MOUSE-XY-GREATER1
  (new Politician%
       [politician-x 230]
       [politician-y 380]
       [mouse-xcoordinate 250]
       [mouse-ycoordinate 400]
       [politician-speed 110]
       [hillary? false]))

(define POLITICIAN-MOUSE-XY-SMALLER1
  (new Politician%
       [politician-x 230]
       [politician-y 380]
       [mouse-xcoordinate 200]
       [mouse-ycoordinate 350]
       [politician-speed 110]
       [hillary? false]))

(define POLITICIAN-MOUSE-X-SMALLER
  (new Politician%
       [politician-x 230]
       [politician-y 380]
       [mouse-xcoordinate 200]
       [mouse-ycoordinate 400]
       [politician-speed 110]
       [hillary? false]))

(begin-for-test
  (check-equal?
   (send (send POLITICIAN-TEST-TOWARDS-MOUSE after-key-event "p") toy-data)
   28 "Politician after key event test failed")
  (check-equal?
   (send (send POLITICIAN-TEST-TOWARDS-MOUSE after-button-down 300 300) toy-data)
   28 "Politician after button down test failed")
  (check-equal?
   (send (send POLITICIAN-TEST-TOWARDS-MOUSE after-button-up 300 300) toy-data)
   28 "Politician after button up test failed")
  (check-equal?
   (send (send POLITICIAN-TEST-TOWARDS-MOUSE after-drag 300 300) toy-data)
   28 "Politician after drag test failed")
  (check-equal?
   (send (send POLITICIAN-TEST-CANVASCENTER after-tick) toy-x)
   288 "Politician test after tick failed for x-coordinate")
  (check-equal?
   (send (send POLITICIAN-TEST-CANVASCENTER after-tick) toy-y)
   338 "Politician test after tick failed for y-coordinate")
  (check-equal?
   (send (send POLITICIAN-TEST-TOWARDS-MOUSE after-tick) toy-y)
   380 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-MOUSE-XY-GREATER after-tick) toy-x)
   170 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-MOUSE-Y-GREATER after-tick) toy-x)
   390 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-MOUSE-XY-GREATER1 after-tick) toy-x)
   120 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-MOUSE-XY-SMALLER1 after-tick) toy-x)
   340 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-MOUSE-X-SMALLER after-tick) toy-x)
   340 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-TEST-2 after-tick) toy-x)
   310 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-TEST-3 after-tick) toy-y)
   310 "Politician test after tick failed")
  (check-equal?
   (send (send POLITICIAN-TEST-3 after-move 300 300) toy-x)
   400 "Politician test after move failed")
  (check-equal?
   (send (send POLITICIAN-TEST-3 after-move 300 300) toy-data)
   100 "Politician test after move failed")
  (check-equal?
   (send (send (new Politician% [politician-x 300] [politician-y 300]  [mouse-xcoordinate 450] [mouse-ycoordinate 450] [politician-speed 10]
                    [hillary? false]) after-tick)toy-x) 
   375 "Politician test after tick failed"))


;; METATOY

(define POLITICIAN-1-IMAGE
  (bitmap "politician1.jpg"))
(define POLITICIAN-2-IMAGE
  (bitmap "politician2.jpg"))


(define TOYLIST-TEST
  (list THROBBER-TEST CLOCK-TEST POLITICIAN-TEST-CANVASCENTER))

(define TOYLIST-TEST-POL2
  (list THROBBER-TEST CLOCK-TEST POLITICIAN-TEST-CANVASCENTER-TRUMP))

(define METATOY-TEST
  (make-metatoy TOYLIST-TEST))

(define METATOY-TEST-POL2
  (make-metatoy TOYLIST-TEST-POL2))

(define IMAGE-FOR-TEST-POLITICIAN-1
  (place-image THROBBER-IMAGE-TEST 250 300
               (place-image (text (number->string 0) 12 "red")
                            250 300
                            (place-image CLOCK-IMAGE-TEST 250 300
                                         (place-image POLITICIAN-1-IMAGE 250 300 EMPTY-CANVAS)))))

(define IMAGE-FOR-TEST-POLITICIAN-2
  (place-image THROBBER-IMAGE-TEST 250 300
               (place-image (text (number->string 0) 12 "red")
                            250 300
                            (place-image CLOCK-IMAGE-TEST 250 300
                                         (place-image POLITICIAN-2-IMAGE 250 300 EMPTY-CANVAS)))))


(begin-for-test
  (check-equal?
   (send (first (send (send METATOY-TEST after-tick) get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after tick test failed for toy-x")
  (check-equal?
   (send (first (send (send METATOY-TEST after-tick) get-toys)) toy-y)
   CANVAS-YCENTER "Metatoy after tick test failed for toy-y")
  (check-equal?
   (send (first (send (send METATOY-TEST after-key-event NEW-THROBBER-EVENT) get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after throbber key event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-key-event NEW-CLOCK-EVENT) get-toys)) toy-y)
   CANVAS-YCENTER "Metatoy after clock key event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-key-event NEW-POLTICIAN-EVENT) get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after politician key event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-key-event "x") get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after any other key event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-mouse-event 60 -10 BUTTON-DOWN) get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after button down mouse event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-mouse-event 60 -10 BUTTON-UP) get-toys)) toy-y)
   CANVAS-YCENTER "Metatoy after button up mouse event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-mouse-event 60 -10 DRAG) get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after drag mouse event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-mouse-event 60 -10 MOVE) get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after move mouse event test failed")
  (check-equal?
   (send (first (send (send METATOY-TEST after-mouse-event 60 -10 "enter") get-toys)) toy-x)
   CANVAS-XCENTER "Metatoy after any other mouse event test failed")
  (check-equal?
   (send METATOY-TEST to-scene)
   IMAGE-FOR-TEST-POLITICIAN-1
   "Metatoy to-scene test failed")
  (check-equal?
   (send METATOY-TEST-POL2 to-scene)
   IMAGE-FOR-TEST-POLITICIAN-2
   "Metatoy to-scene test failed"))