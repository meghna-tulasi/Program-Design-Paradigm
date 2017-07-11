;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "07" "q2.rkt")


(provide
 probe-possible-outcome?
 make-turn-left
 make-turn-right
 make-move-forward)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct program (loi))
;A Program is a ListOfInstruction
;Interp: A sequence of instructions, to be executed from left to
;right. 
;

;;TEMPLATE:
;;program-fn: Program -> ?
;(define (program-fn p)
;  (loi-fn p))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ListOfInstruction is one of the following:
;;--empty
;;--(cons Instruction LOI)

;;TEMPLATE:
;;loi-fn :LOI -?
;(define (loi-fn loi)
;  (cond
;    [(empty? loi)...]
;    [else (...
;           (ins-fn (first loi))
;           (loi-fn (rest loi)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;An Instruction is one of
;-- (make-turn-left)            Interp: a turn-left instruction
;-- (make-turn-right)           Interp: a turn-right instruction
;-- (make-move-forward PosInt)  Interp: an instruction to move forward
;                                       the given number of steps.

;TEMPLATE:
;;ins-fn : Instruction -> ??
#;
(define (ins-fn i)
  (cond
    [(turn-left? i) (...(turn-left i))]
    [(turn-right? i)...(turn-right i)]
    [(move-forward? i)...(move-forward-n i)]))

(define-struct turn-left())
;;turn-left is
;;a empty structure
;;(make-turn-left)

(define-struct turn-right())
;;turn-right is
;;a empty structure
;;(make-turn-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct move-forward(n))
;;move-forward is
;;(make-move-forward PosInteger)
;;INTERPRETATION:
;;n -> number of steps the probe has to move

;;TEMPLATE:
;;move-forward-fn : move-forward -> ??
;;(define (move-forward-fn move-forward)
;;  (...
;;   (move-forward-n move-forward)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct probe(xi yi xf yf direction))
 
;;Probe is a
;;(make-probe Integer Integer Integer Integer String)
;;INTERPRETATION:
;;xi -> the smallest x-coordinate of probe
;;yi -> the smallest y-coordinate of probe
;;xf -> the largest x-coordinate of probe
;;yf -> the largest y-coordinate of probe
;;direction is the direction to which probe faces and takes value from:
;;north,south,east,west.

;;TEMPLATE:
;;probe-fn : Probe -> ??
;(define (probe-fn pb)
;  (...
;   (probe-xi pb)
;   (probe-yi pb)
;   (probe-xf pb)
;   (probe-yf pb)
;   (probe-direction pb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
(define p1
  (list (make-turn-right)
        (make-move-forward 10)
        (make-turn-right)
        (make-move-forward 5)))

(define N "north")
(define S "south")
(define E "east")
(define W "west")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION DEFIFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-possible-outcome? : Int Int Program Int Int -> Boolean
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome? 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].
;; STRATEGY : Use the template of probe

(define (probe-possible-outcome? x0 y0 p x1 y1)
  (check-range? x1 y1 (process-instructions (make-probe x0 y0 x0 y0 N) p)))  

;; TESTS
(begin-for-test
  (check-equal?
   (probe-possible-outcome?
    20 100 (list (make-turn-right) (make-move-forward 5) (make-turn-right) (make-move-forward 5)) 0 0)
   false
   "The output for this test should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-instructions: Probe Program -> Probe
;; GIVEN: a probe and a program
;; RETURNS: a probe after all of instructions are processed
;; EXAMPLES:(process-instructions (make-probe 20 100 20 100 N) p1)
;; ->(make-probe 28 103 32 107 S)
;; HALTING MEASURE: the number of elements in program
;; STRATEGY: Use the template of ListOfInstructions
(define (process-instructions probe p)
  (if (empty? p)
      probe
      (process-instructions (process-instruction probe (first p)) (rest p))))
;; TEST
(begin-for-test
  (check-equal?
   (process-instructions (make-probe 20 100 20 100 N) p1)
   (make-probe 28 103 32 107 S)
   "The output should be probe with attributes 20 103 32 107 'south'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-instruction : Probe Instruction -> Probe
;; GIVEN: a probe and an Instruction
;; RETURNS: a probe after the instruction has been processed.
;; EXAMPLES: (process-instruction (make-probe 20 100 20 100 N) (make-turn
;; right)) -> (make-probe 20 100 20 100 E)
;; STRATEGY: Divide into cases on i

(define (process-instruction probe i)
  (cond
    [(turn-left? i) (probe-direction-left probe)]
    [(turn-right? i) (probe-direction-right probe)]
    [(move-forward? i) (probe-move-forward  probe (move-forward-n i))]))

;; TEST
(begin-for-test
  (check-equal?
   (process-instruction (make-probe 20 100 20 100 N) (make-turn-right))
   (make-probe 20 100 20 100 E)
   "The direction of the output probe should be east")
  
  (check-equal?
   (process-instruction (make-probe 20 100 20 100 N) (make-turn-left))
   (make-probe 20 100 20 100 W)
   "The direction of the output probe should be west")
  
  (check-equal?
   (process-instruction (make-probe 20 100 20 100 N) (make-move-forward 5))
   (make-probe 20 93 20 97 N)
   "The attributes of the probe should be 20 102 20 107 'north'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probe-move-forward Probe PosInt -> Probe
;; GIVEN: a probe and positive number
;; RETURNS: a probe after move forward according to the given number
;; EXAMPLES: (probe-move-forward (make-probe 20 100 20 100 N) 5)
;; -> (make-probe 20 93 20 97 N)
;; STRATEGY: Divide into cases on the direction of probe
(define (probe-move-forward pb n)
  (cond
    [(string=? (probe-direction pb) N) (move-forward-north pb n)]
    [(string=? (probe-direction pb) E) (move-forward-east pb n)]
    [(string=? (probe-direction pb) S) (move-forward-south pb n)]
    [(string=? (probe-direction pb) W) (move-forward-west pb n)]))

;; TEST
(begin-for-test
  (check-equal?
   (probe-move-forward (make-probe 20 100 20 100 N) 5)
   (make-probe 20 93 20 97 N)
   "The attributes of probe should be 20 93 20 97 'north'")
  
  (check-equal?
   (probe-move-forward (make-probe 20 100 20 100 E) 5)
   (make-probe 23 100 27 100 E)
   "The attributes of probe should be 20 93 20 97 'east'")
  
  (check-equal?
   (probe-move-forward (make-probe 20 100 20 100 W) 5)
   (make-probe 13 100 17 100 W)
   "The attributes of probe should be 20 93 20 97 'west'")
  
  (check-equal?
   (probe-move-forward (make-probe 20 100 20 100 S) 5)
   (make-probe 20 103 20 107 S)
   "The attributes of probe should be 20 93 20 97 'south'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-forward-north: Probe PosInt -> Probe
;; GIVEN: a probe and a positive number
;; RETURNS: a probe after move forward towards north
;; EXAMPLES: (move-forward-north (make-probe 20 100 20 100 N) 5)
;; -> (make-probe 20 93 20 97 N)
;; STRATEGY: Use the template of probe
(define (move-forward-north pb n)
  (make-probe (probe-xi pb) (- (- (probe-yi pb) n) 2)
              (probe-xf pb) (+ (- (probe-yf pb) n) 2) (probe-direction pb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-forward-south: Probe PosInt -> Probe
;; GIVEN: a probe and a positive number
;; RETURNS: a probe after move forward towards south
;; EXAMPLES: (move-forward-south (make-probe 20 100 20 100 S) 5)
;; -> (make-probe 20 103 20 107 N)
;; STRATEGY: Use the template of probe
 (define (move-forward-south pb n)
  (make-probe (probe-xi pb) (- (+ (probe-yi pb) n) 2)
              (probe-xf pb) (+ (+ (probe-yf pb) n) 2) (probe-direction pb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-forward-east: Probe PosInt -> Probe
;; GIVEN: a probe and a positive number
;; RETURNS: a probe after move forward towards east
;; EXAMPLES: (move-forward-east (make-probe 20 100 20 100 E) 5)
;; -> (make-probe 23 100 27 100 E)
;; STRATEGY: Use the template of probe
 (define (move-forward-east pb n)
   (make-probe  (- (+ (probe-xi pb) n) 2) (probe-yi pb)
                (+ (+ (probe-xf pb) n) 2) (probe-yf pb) (probe-direction pb))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-forward-west: Probe PosInt -> Probe
;; GIVEN: a probe and a positive number
;; RETURNS: a probe after move forward towards west
;; EXAMPLES: (move-forward-west (make-probe 20 100 20 100 N) 5)
;; -> (make-probe 13 100 17 100 W)
;; STRATEGY: Use the template of probe
 (define (move-forward-west pb n) 
  (make-probe  (- (- (probe-xi pb) n) 2) (probe-yi pb)
               (+ (- (probe-xf pb) n) 2) (probe-yf pb) (probe-direction pb)))  
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probe-direction-right: Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe after changing direction towards right
;; EXAMPLES: (probe-direction-right (make-probe 20 100 20 100 N)
;; -> (make-probe 20 100 20 100 E)
;; STRATEGY: Use the template of probe

(define (probe-direction-right pb)
  (cond
      [(string=? (probe-direction pb) N)
       (make-probe (probe-xi pb) (probe-yi pb) (probe-xf pb) (probe-yf pb)  E)]
      [(string=? (probe-direction pb) E)
       (make-probe (probe-xi pb) (probe-yi pb) (probe-xf pb) (probe-yf pb) S)]
      [(string=? (probe-direction pb) S)
       (make-probe (probe-xi pb) (probe-yi pb) (probe-xf pb) (probe-yf pb) W)]
      [(string=? (probe-direction pb) W)
       (make-probe (probe-xi pb) (probe-yi pb)  (probe-xf pb) (probe-yf pb) N)]))

;; TEST
(begin-for-test
  (check-equal?
   (probe-direction-right (make-probe 20 100 20 100 S))
   (make-probe 20 100 20 100 W)
   "The direction of the output probe should be west")
  (check-equal?
   (probe-direction-right (make-probe 20 100 20 100 W))
   (make-probe 20 100 20 100 N)
   "The direction of the output probe should be north"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probe-direction-left: Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe after changing direction towards left
;; EXAMPLES: (probe-direction-right (make-probe 20 100 20 100 N)
;; -> (make-probe 20 100 20 100 W)
;; STRATEGY: Use the template of probe
(define (probe-direction-left pb)
  (cond
      [(string=? (probe-direction pb) N)
       (make-probe (probe-xi pb) (probe-yi pb) (probe-xf pb) (probe-yf pb) W)]
      [(string=? (probe-direction pb) W) 
       (make-probe (probe-xi pb) (probe-yi pb)  (probe-xf pb) (probe-yf pb)S)]
      [(string=? (probe-direction pb) S)
       (make-probe (probe-xi pb) (probe-yi pb) (probe-xf pb) (probe-yf pb) E)]
      [(string=? (probe-direction pb) E)
       (make-probe (probe-xi pb) (probe-yi pb)  (probe-xf pb) (probe-yf pb) N)]))

;; TEST
(begin-for-test
  (check-equal?
   (probe-direction-left (make-probe 20 100 20 100 S))
   (make-probe 20 100 20 100 E)
   "The direction of the output probe should be east")
  (check-equal?
   (probe-direction-left (make-probe 20 100 20 100 W))
   (make-probe 20 100 20 100 S)
   "The direction of the output probe should be south")
  (check-equal?
  (probe-direction-left (make-probe 20 100 20 100 N))
   (make-probe 20 100 20 100 W)
   "The direction of the output probe should be west")
  (check-equal?
   (probe-direction-left (make-probe 20 100 20 100 E))
   (make-probe 20 100 20 100 N)
   "The direction of the output probe should be north"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-coordinates?  : Int Int Int Int PosInt -> Boolean
;; GIVEN : Intial and final coordinates of the probe and number of steps it needs to move
;; RETURNS : true if final coordinates of probe are in the range of +2 or -2
;;           after the addition of steps
;; EXAMPLES : (check-coordinates? 20 100 29 100 10) -> true
;; STRATEGY : Use the template of probe

(define (check-range? x1 y1 pb)
  (and
   (and (>= x1 (probe-xi pb)) (<= x1 (probe-xf pb)))
   (and (>= y1 (probe-yi pb)) (<= y1 (probe-yf pb)))))

;; TEST
(begin-for-test
  (check-equal?
   (check-range? 23 107 (make-probe 22 103 25 106 N))
   false
   "This location is not in the range of possible probe"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  

;;;; scalable-stress-program : Int -> Program
;;;; Given: an integer n
;;;; Returns: a Program of length (* 6 (max 0 n))
;
;(define (scalable-stress-program n)
;  (if (> n 0)
;      (append (list (make-turn-right)
;                    (make-move-forward 10)
;                    (make-turn-right)
;                    (make-move-forward 10)
;                    (make-turn-left)
;                    (make-turn-left))
;              (scalable-stress-program (- n 1)))
;      empty))
;
;;;; stress-benchmark : Int -> Boolean
;;;; Given: an integer, preferably non-negative, stating the desired
;;;;     degree of difficulty for the benchmark
;;;; Returns: true
;;;; Effect: reports how much time probe-possible-outcome?
;;;;     takes to compute the correct answer
;
;(define (stress-benchmark n)
;  (let ((pgm (scalable-stress-program n)))
;    (time (probe-possible-outcome? 0 0 pgm (* 10 n) (* 10 n)))))
;











