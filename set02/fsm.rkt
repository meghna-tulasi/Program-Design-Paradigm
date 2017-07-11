;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")

(provide
 initial-state
 next-state
 accepting-state?
 error-state?
 )

 

;; DATA DEFINITION:

;; A state is one of the following
;; -- "A"(INITIAL STATE)
;; -- "B"
;; -- "C"
;; -- "D"
;; -- "ER" error state

;; finite-state-model-fn : state -> ??
#;(define (finite-state-model-fn st)
  (cond
    [(string=? (state-current-state st) "A") ...]
   [(string=?  (state-current-state st) "B") ...]
   [(string=?  (state-current-state st) "C") ...]
   [(string=?  (state-current-state st) "D") ...]
   [(string=?  (state-current-state st) "ER") ...]))
   
  (define-struct state(current-state input))
  
;;State is
  ;;(make-state finitestatemodel string)
;;where curent state is a string and may be one of the 5 states
;;input is the input string that has to be parsed accross states

;; state-fn : State -> ??
(define (state-fn state)
 (...
  state-current-state
  state-input))
   


;;INITIAL STATE
;;initial-state : Number -> State
;;GIVEN: a number
;;RETURNS: a representation of the initial state
;;of your machine.  The given number is ignored.

;;EXAMPLES:
;; (initial-state 204) ="A"
;; (initial-state 4/3) ="A"
;; (initial-state -6.7) ="A"

;;DESIGN STRATEGY:Composing a Function

(define (initial-state no)
(make-state "A" ""))



           

;;NEXT-STATE:


;;next-state : State MachineInput -> State
;;GIVEN: a state of the machine and a machine input
;;RETURNS: the state that should follow the given input.

;;EXAMPLES:
;; (next-state (initial-state 1) "q") -> (make-state "A" "q")
;;(next-state (make-state "A" "q") "u") ->(make-state "C" "qu")
;;(next-state (make-state "C" "qu") "d") ->(make-state "D" "qud")
;;(next-state (make-state "D" "qud") "u") ->(make-state "ER" "qud")

;;DESIGN STRATEGY: Dividing into cases

(define (next-state st mi)
  (cond
    [(string=? (state-current-state st) "A") (s0-state st mi)]
    [(string=? (state-current-state st) "B") (s1-state st mi)]
    [(string=? (state-current-state st) "C") (s2-state st mi)]
    [(string=? (state-current-state st) "D") (s3-state st mi)]
    [(string=? (state-current-state st) "ER") (new-state st "ERROR-STATE" mi)]))



;;s0-state: State MachineInput -> State
;; GIVEN: state and machine input
;; RETURNS: A,B,C,D State or Error State
;; EXAMPLES:
 ;; (s0-state (make-state "A" "") "q") -> (make-state "A" "q")
 ;; (s0-state (make-state "A" "") "x") -> (make-state "A" "x")
 ;; (s0-state (make-state "A" "") "u") -> (make-state "C" "u")
 ;; (s0-state (make-state "A" "") "d") -> (make-state "B" "d")
;; (s0-state (make-state "A" "") "a") -> (make-state "D" "a")
;; (s0-state (make-state "A" "") "b") -> (make-state "D" "b")
;; (s0-state (make-state "A" "") "e") -> (make-state "ER" "e")
;; (s0-state (make-state "A" "") "f") -> (make-state "ER" "f")

 
;; STRATEGY: Divide into cases based on machine input mi
(define (s0-state st mi)
  (cond
    [(string=? mi "u") (new-state st "C" mi)]
    [(string=? mi "d") (new-state st "B" mi)]
    [(string=? mi "a") (new-state st "D" mi)]
    [(string=? mi "b") (new-state st "D" mi)]
    [(string=? mi "q") (new-state st "A" mi)]
    [(string=? mi "x") (new-state st "A" mi)]
    [(string=? mi "e") (new-state st "ER" mi)]
    [(string=? mi "f") (new-state st "ER" mi)]))
      

;;s1-state: State MachineInput -> State
;; GIVEN: state and machine Input
;; RETURNS:State B or Error State
;; EXAMPLES:
;; (s1-state (make-state "B" "d") "e" => (make-state "B" "de")
;; (s1-state (make-state "B" "e") "e") => (make-state "B" "ee")
;; (s1-state (make-state "B" "f") "f") => (make-state "B" "ff")
;; (s1-state (make-state "B" "d") "f") => (make-state "B" "df")
;; (s1-state (make-state "B" "d") "q") => (make-state "ER" "dq")
;; (s1-state (make-state "B" "d") "u") => (make-state "ER" "du")
;; (s1-state (make-state "B" "d") "x") => (make-state "ER" "dx")
;; (s1-state (make-state "B" "d") "a") => (make-state "ER" "da")
;; (s1-state (make-state "B" "d") "b") => (make-state "ER" "db")

;; STRATEGY: Divide into cases based on machine input mi
(define (s1-state st mi)
  (cond
    
    [(string=? mi "e") (new-state st "B" mi)]
    [(string=? mi "f") (new-state st "B" mi)]
    [else (new-state st "ER" mi)]))

    
;;s2-state: State MachineInput -> State
;; GIVEN: state and machine Input
;; RETURNS:State B,C ,D or Error state
;; EXAMPLES:
;; (s2-state (make-state "C" "qu") "u" => (make-state "C" "quu")
;; (s2-state (make-state "C" "qx") "u") => (make-state "C" "qxu")
;; (s2-state (make-state "C" "xu") "u") => (make-state "C" "xuu")
;; (s2-state (make-state "C" "uu") "u") => (make-state "C" "uuu")
;; (s2-state (make-state "C" "qu") "d") => (make-state "B" "qud")
;; (s2-state (make-state "C" "qu") "a") => (make-state "D" "qua")
;; (s2-state (make-state "C" "qu") "b") => (make-state "D" "qub")
;; (s2-state (make-state "C" "uu") "d") => (make-state "B" "uud")



;; STRATEGY: Divide into cases based on machine input mi
(define (s2-state st mi)
  (cond
    [(string=? mi "u") (new-state st "C" mi)]
    [(string=? mi "d") (new-state st "B" mi)]
    [(string=? mi "a") (new-state st "D" mi)]
    [(string=? mi "b") (new-state st "D" mi)]
    [else (new-state st "ER" mi)]))


;;s3-state: State MachineInput -> State
;; GIVEN: state and machine Input
;; RETURNS:State B, D or Error state
;; EXAMPLES:
;; (s3-state (make-state "D" "qua") "a" => (make-state "D" "quaa")
;; (s3-state (make-state "D" "qub") "b" => (make-state "D" "qubb")
;; (s3-state (make-state "D" "qua") "b" => (make-state "D" "quab")
;; (s3-state (make-state "D" "qua") "d" => (make-state "B" "quad")

;; STRATEGY: Divide into cases based on machine input mi
(define (s3-state st mi)
  (cond
    [(string=? mi "a") (new-state st "D" mi)]
    [(string=? mi "b") (new-state st "D" mi)]
    [(string=? mi "d") (new-state st "B" mi)]
    [else (new-state st "ER" mi)]))

;;ACCEPTING-STATE
;;accepting-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true iff the given state is a final (accepting) state
;;EXAMPLES:
;; (accepting-state? (make-state "B" "d")) -> true
;; (accepting-state? (make-state "C" "qxu")) -> false
;;DESIGN STRATEGY: Using Template

(define (accepting-state? st)
  (string=? "B" (state-current-state st)))
  

;;ERROR-STATE:

;;error-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true iff there is no path (empty or non-empty) from the given
;;state to an accepting state

;;EXAMPLES:
;; (error-state? (make-state "ERROR" "qxudu")) -> true
;;(error-state? (make-state "B" "qxd")) -> false


;;DESIGN STRATEGY: Using Template

(define (error-state? st)
  (string=? "ERROR" (state-current-state st)))


;;new-state: State FSM MachineInput -> State
;; GIVEN: Current state, new state which is to be created
;;and Machine Input which will transit to the new state
;; RETURNS: a new state
;; EXAMPLES:
;; (create-state (make-state "INITIAL" "") "A" "q") => (make-state "A" "q")
;; STRATEGY: Use template of State on st
(define (new-state st s mi)
  (make-state s (string-append (state-input st) mi)))


;;TESTS:
(begin-for-test
  (check-equal? (state? (initial-state 4)) true
                "Should return true")
  (check-equal? (state-current-state (next-state (initial-state 4) "q")) "A"
                "Should return state A")
  (check-equal? (state-current-state (next-state (initial-state 4) "e")) "ER"
                "Should return error state")
  
  (check-equal? (state-current-state (next-state (make-state "A" "q") "u"))"C"
                "Next State Should be C")
  (check-equal? (state-current-state (next-state (make-state "C" "qu") "d"))"B"
                "Next State Should be B")
  (check-equal? (state-current-state (next-state (make-state "B" "qud") "e"))"B"
                "Next State Should be B")
  (check-equal? (state-current-state (next-state (make-state "B" "qude") "u")) "ER"
                 "Should return Error State")
  (check-equal? (state-current-state (next-state (make-state "A" "qxuaabb") "d")) "B"
                 "Should return B state")
  (check-equal? (state-current-state (next-state (make-state "C" "qxu") "u")) "C"
                 "Should return state C")
  (check-equal? (state-current-state (next-state (make-state "D" "qxu") "a")) "D"
                 "Should return state D")
  (check-equal? (state-current-state (next-state (make-state "D" "qxuaa") "b")) "D"
                   "Should return state D")
  (check-equal? (state-current-state (next-state (make-state "A" "qqx") "x")) "A"
                   "Should return state A")
  (check-equal? (state-current-state (next-state (make-state "B" "qqxdee") "f")) "B"
                   "Should return state B")
  (check-equal? (state-current-state (next-state (make-state "B" "qqxdee") "a")) "ER"
                   "Should return state ERROR-STATE")
  (check-equal? (state-current-state (next-state (make-state "ER" "qqxdee") "a")) "ERROR-STATE"
                   "Should return state ERROR-STATE")
  (check-equal? (state-current-state (next-state (make-state "A" "abb") "a"))"D"
                 "Should return state D")
  (check-equal? (state-current-state (next-state (make-state "A" "bb") "b"))"D"
                 "Should return state D")
  (check-equal? (state-current-state (next-state (make-state "A" "qx") "f"))"ER"
                 "Should return error state")
  (check-equal? (error-state? (next-state (make-state "D" "qqxxuab") "d")) false
                 "Should return false")
  (check-equal? (state-current-state (next-state (make-state "C" "qqu") "a"))"D"
                "Should return state D")
  (check-equal? (state-current-state (next-state (make-state "C" "qqua") "b"))"D"
                "Should return state D")
  (check-equal? (accepting-state? (next-state (next-state (next-state
                 (initial-state 1) "q") "u") "d")) true "Should return true")
  (check-equal? (state-current-state (next-state (make-state "C" "u") "e")) "ER"
               "Should return error-state")
  (check-equal? (state-current-state (next-state (make-state "D" "u") "e")) "ER"
               "Should return error-state"))
  
  
    
  
  
                
  
  
  
  
  
  
  
           























