;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 initial-state
 next-state)


;; DATA DEFINITION:
;; A state is one of the following
;; -- "A"
;; -- "B"
;; -- "C"
;; -- "D"

;; INTERPRETATION: self-evident

;; TEMPLATE
;; state-fn : State -> ??

#|
(define (state-fn state)
 (cond
   [(string=? state "A")    ...]
   [(string=? state "B")    ...]

   [(string=? state "C") ...]
   [(string=? state "D")  ...]))  
|#



;;initial-state : Number -> State
;;GIVEN: a number
;;RETURNS: a representation of the initial state
;;of your machine.  The given number is ignored.

(define (initial-state no)
 "A")


(begin-for-test
  (check-equal? (initial-state 1) "A")
  (check-equal? (initial-state 2300) "A")
  (check-equal? (initial-state -128) "A")
  (check-equal? (initial-state 3.2) "A"))

           




;;next-state : State MachineInput -> State
;;GIVEN: a state of the machine and a machine input
;;RETURNS: the state that should follow the given input.
(define (next-state state machineinput)
  (cond
    
    [(or q x) 

      (string=? state "A" "q") "A"]
    [(string=? state "A" "x") "A"]
     [(string=? state "A" "u") "C"]
      [(string=? state "C" "u") "C"]
       [(string=? state "A" "d") "B"]
        [(string=? state "A" "a") "D"]
         [(string=? state "A" "b") "D"]
          [(string=? state "D" "a") "D"]
           [(string=? state "D" "b") "D"]))



    

    