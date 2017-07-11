;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")
(provide initial-machine
         machine-next-state
         machine-output
         machine-remaining-kale
         machine-remaining-carrots
         machine-bank)

;;Defining the price of kale chips and carrot sticks in cents
(define kale 0.75)
(define carrots 0.50)

;;DATA DEFINITIONS:

;;CustomerInput is one of the following:
 ;;--PosInt : Inserted amount/quarters,in cents
 ;;--Kale : requests bag of kale chips
 ;;--carrot : requests bag of carrot sticks
 ;;--change : requests unspent money that customer had inserted

;;INTERPRETATION: Customer inserts money or requests for kale or carrot bags.
                  ;;or can request unspent money.

;;TEMPLATE
;; customer-input-fn :CustomerInput -> ??
#;(define (customer-input-fn ci)
    (cond
     [(integer? ci)...]
     [(string=? ci "kale")...]
     [(string=? ci "carrots")...]
     [(string=? ci "change/unspent-money")...]))





(define-struct machine-state(kalebags carrotbags bank amount machine-output))

;;MachineState is
  ;;(make-machine-state NonNegInt NonNegInt NonNegInt NonNegInt MachineOutput)
;;Interpretation:
  ;;kalebags is the total number of kale chips available in the vending machine.
  ;;carrotbags is the total number of carrot sticks available in the vending machine.
  ;;amount in cents is the total money in the vending machine to dispense kale and
  ;;carrot bags at that particular machine state,
  ;;machine-output is the output of the vending machine for a particular customer input.

;;machine-state-fn :MachineState -> ??
#;(define (machine-state-fn ms)
  (...
   (machine-state-kalebags ms)
   (machine-state-carrotbags ms)
   (machine-state-bank ms)
   (machine-state-amount ms)
   (machine-state-machine-output ms)))



;;initial-machine : NonNegInt NonNegInt -> MachineState
;;GIVEN: a number of bags of kale chips and carrot sticks
;;RETURNS: the state of a machine loaded with the given numbers of bags
;;of kale chips and carrot sticks, with an empty bank.
;;EXAMPLEs:
  ;;(initial-machine 3 2)=(make-machine-state 3 2 0 0 "Nothing")
  ;;(initial-machine 0 1)=(make-machine-sate 0 1 0 0 "Nothing")
;;DESIGN STRATEGY:Using the constructor template of machine-state
(define (initial-machine x y)
(make-machine-state x y 0 0 "Nothing"))
     





;;machine-next-state : MachineState CustomerInput -> MachineState
;;GIVEN: a machine state and a customer input
;;RETURNS: the state of the machine that should follow the customer's input.
;;EXAMPLES:              
  ;;(machine-next-state (make-machine-state 3 2 0 100 "Nothing") "kale" ->
                                     ;;(make-machine-state 2 2 0 100 "kale")
  ;;(machine-state (make-machine-state 4 4 0 1000 "Nothing") "kale" "carrots" ->
                                    ;;(make-machine-state 3 3 0 100 "kale" "carrots")

;;DESIGN-STRATEGY:Using template of customer-input
(define (machine-next-state  ms ci)
  (cond
    [(integer? ci) (add-quarters ms ci)]
    [(string=? ci "kale") (dispense-kale ms)]
    [(string=? ci "carrots") (dispense-carrots ms)]
    [(string=? ci  "change") (return-unspent-money ms ci)]))



;;machine-output : MachineState CustomerInput -> MachineOutput
;;GIVEN: machine state and customer input
;;RETURNS: machine output for a particular customer input.
;;EXAMPLES:
  ;;(machine-output (make-machine-state 0 3 0 0 "Nothing")"kale") -> "Out of Items"
  ;;(machine-output (make-machine-state 3 3 0 100 "Nothing")"carrots") -> "carrots"
;;DESIGN-STRATEGY: Composing Functions
(define (machine-output ms ci)
(machine-state-machine-output (machine-next-state ms ci)))




;;add-quarters: MachineState NonNegInt -> MachineState
;;GIVEN: machine state and amount inserted
;;RETURNS: MachineState after the vending machine is updated with amount inserted by
           ;;customer, in cents
;;EXAMPLES:
   ;;(add-quarters (make-machine-state 3 3 100 100 "kale") 3)
   ;;-> (make-machine-state 3 3 103 100 "Nothing")
;;DESIGN-STRATEGY:Using template of machine-state
(define (add-quarters ms q)
  (make-machine-state (machine-state-kalebags ms) (machine-state-carrotbags ms)
   (machine-state-bank ms) (+ (machine-state-amount ms) q)  "Nothing"))



;;enough-kale? : MachinesState -> Boolean
;;GIVEN: a machine state
;;RETURNS: true or false 
;;EXAMPLES:
 ;;(enough-kale? (make-machine-state 0 3 5 0 "Nothing")) -> false
;;DESIGN-STRATEGY:Using template of machine-state
(define (enough-kale? ms)
  (> (machine-state-kalebags ms) 0))


;;enough-amount-kale? : MachineState -> Boolean
;;GIVEN: a machine state
;;RETURNS: true or false 
;;EXAMPLES:
 ;;(enough-amount-kale? (make-machine-state 2 3 0 5 "Nothing")) -> true
;;DESIGN-STRATEGY:Using templateof machine-state
(define (enough-amount-kale? ms)
  (>= (machine-state-amount ms) kale))

;;enough-carrots? : MachinesState -> Boolean
;;GIVEN: a machine state
;;RETURNS: true or false 
;;EXAMPLES:
 ;;(enough-carrots? (make-machine-state 3 0 5 0 "Nothing")) -> false
;;DESIGN-STRATEGY:Using template of machine-state
(define (enough-carrots? ms)
  (> (machine-state-carrotbags ms) 0))


;;enough-amount-carrots? : MachineState -> Boolean
;;GIVEN: a machine state
;;RETURNS: true or false 
;;EXAMPLES:
 ;;(enough-amount-carrots? (make-machine-state 2 3 0 0.25 "Nothing")) -> false
;;DESIGN-STRATEGY:Using template of machine-state
(define (enough-amount-carrots? ms)
  (>= (machine-state-amount ms) carrots))


;;dispense-item-kale: MachineState -> MachineState
;;GIVEN: Current Machine State
;;RETURNS: New Machine State having output kale.
;;EXAMPLES:
 ;;(dispense-item-kale (make-machine-state 3 10 0 1 "Nothing") "kale") ->
                ;;(make-machine-state 2 10 0 1 "kale")
;;DESIGN-STRATEGY: Using template of machine-state
(define (dispense-item-kale ms)
  (make-machine-state ( - (machine-state-kalebags ms) 1) (machine-state-carrotbags ms)
                      (+ (machine-bank ms) kale) (- (machine-state-amount ms) kale) "kale dispensed")) 

;;dispense-kale: MachineState -> MachineState
;;GIVEN: Current Machine State
;;RETURNS: New Machine State having output kale or nothing or Out of Item
;;EXAMPLES:
 ;;(dispense-kale (make-machine-state 3 10 0 1 "Nothing") "kale") -> 
                ;;(make-machine-state 2 10 0 1 "kale")
;;DESIGN-STRATEGY: Dividing into cases 
(define (dispense-kale ms)
  (cond
    [(and (enough-amount-kale? ms) (enough-kale? ms)) (dispense-item-kale ms)]
    [(not (enough-amount-kale? ms)) ms]
    [(and (enough-amount-kale? ms) (not (enough-kale? ms))) (Out-of-Item ms)]))
    


;;dispense-item-carrots: MachineState -> MachineState
;;GIVEN: Current Machine State
;;RETURNS: New Machine State having output carrot.
;;EXAMPLES:
 ;;(dispense-item-carrots (make-machine-state 3 10 0 1 "Nothing") "carrots") ->
                ;;(make-machine-state 3 9 0 1 "carrots")
;;DESIGN-STRATEGY: Using template of machine-state
(define (dispense-item-carrots ms)
  (make-machine-state (machine-state-kalebags ms) (- (machine-state-carrotbags ms) 1)
  (+ (machine-bank ms) carrots) (- (machine-state-amount ms) carrots) "carrots dispensed")) 


;;dispense-carrots: MachineState -> MachineState
;;GIVEN: Current Machine State
;;RETURNS: New Machine State having output carrots or nothing or Out of Item
;;EXAMPLES:
 ;;(dispense-carrots (make-machine-state 3 10 0 1 "Nothing") "carrots") ->
 ;;(make-machine-state 2 10 0 10 "carrots")
;;DESIGN-STRATEGY: Dividing into cases 
(define (dispense-carrots ms)
  (cond
    [(and (enough-amount-carrots? ms) (enough-carrots? ms)) (dispense-item-carrots ms)]
    [(not (enough-amount-carrots? ms)) ms]
    [(and (enough-amount-carrots? ms) (not (enough-carrots? ms))) (Out-of-Item ms)]))

;;return-unspent-money: MachineState NonNegInt -> MachineState
;;GIVEN: Current Machine State and amount inserted by customer to buy item
;;RETURNS:New Machine State wherein any unspent money is returned.
;;EXAMPLES: (dispense-unspent-money (make-machine-state 3 3 75 1 "kale")) ->
           ;;(make-machine-state 3 3 75 0 75)
;;DESIGN-STRATEGY: Using template of machine-state
(define (return-unspent-money ms a)
  (make-machine-state (machine-state-kalebags ms) (machine-state-carrotbags ms)
  (machine-state-bank ms) 0 (machine-state-amount ms)))
   


;;machine-remaining-kale : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of bags of kale chips left in the machine
;;EXAMPLES:
 ;;(machine-remaining-kale (make-machine-state 3 3 0 0 "Nothing")) -> 3
;;DESIGN-STRATEGY: Using template of machine-state
(define (machine-remaining-kale ms)
  (machine-state-kalebags ms))

;;machine-remaining-carrots : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of bags of carrots left in the machine
;;EXAMPLES:
 ;;(machine-remaining-carrots (make-machine-state 3 3 0 0 "Nothing")) -> 3
;;DESIGN-STRATEGY: Using template of machine-state
(define (machine-remaining-carrots ms)
  (machine-state-carrotbags ms))

  
;;machine-bank : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the amount of money in the machine's bank, in cents
;;EXAMPLES:
  ;;(machine-bank (make-machine-state 2 2 0 0 "Nothing")) -> 0
;;DESIGN-STRATEGY: Using template of machine-state

(define (machine-bank ms)
  (machine-state-bank ms))



;;Out-of-Item : MachineState -> MachineState
;; GIVEN: current machine state
;; RETURNS: new machine state with output "Out of Item"
;; EXAMPLES:
;; (Out-of-Item (make-machine-state 10 10 0 10 "Nothing")) -> (make-machine-state 0 10 0 10 "Out of Item")
;; DESIGN-STRATEGY: Using template of machine-state
(define (Out-of-Item ms)
  (make-machine-state (machine-state-kalebags ms) (machine-state-carrotbags ms)
                      (machine-state-bank ms) (machine-state-amount ms)  "Out of Item"))
  
;;TESTS:

(begin-for-test
  (check-equal? (machine-state? (initial-machine 3 3)) true
                "Should return object of Machine State")
  (check-equal? (machine-state-kalebags (initial-machine 3 3)) 3
                "Number of Kale chips bags should be 3")
  (check-equal? (machine-state-carrotbags (initial-machine 2 4)) 4
                "Number of Carrot stick bags should be 4")
  (check-equal? (machine-state-amount(initial-machine 2 4)) 0
                "Should return 0")
  (check-equal? (machine-bank (initial-machine 2 4)) 0
                "Should return 0")
  (check-equal? (machine-state-machine-output (initial-machine 2 4)) "Nothing"
                "Should return Nothing")
  (check-equal? (machine-state-machine-output (machine-next-state (machine-next-state
           (initial-machine 2 4) 10) "kale")) "kale dispensed"  "Should return kale dispensed")
  (check-equal? (machine-state-machine-output (machine-next-state (machine-next-state
           (initial-machine 2 4) 10) "carrots")) "carrots dispensed"  "Should return carrots dispensed")
  (check-equal? (machine-state-machine-output (machine-next-state (machine-next-state
           (initial-machine 2 0) 10) "carrots")) "Out of Item"  "Should return Out of Item")
  (check-equal?(machine-state-amount (machine-next-state (initial-machine 2 4) 9)) 9
               "Should return 9")
  (check-equal?(machine-output (machine-next-state (machine-next-state (initial-machine 10 19)
               1) "kale") "change")0.25 "Should return 0.25 change")
  (check-equal?(machine-output (machine-next-state (machine-next-state (initial-machine 10 19)
               1) "carrots") "change")0.5 "Should return 0.5 change")
  (check-equal?(machine-output (machine-next-state (initial-machine 0 9) 1) "kale")
               "Out of Item" "Should return Out of Item")
  (check-equal? (machine-bank (machine-next-state (machine-next-state (initial-machine 2 4)
              1) "kale")) 0.75 "0.75 cents will be added in the bank")
  (check-equal? (machine-state-amount(machine-next-state (machine-next-state (initial-machine
             2 4) 4) "carrots")) 3.5 "3.5$ is the amount in vending machine during transaction")
  
  (check-equal? (machine-remaining-kale (machine-next-state (machine-next-state (initial-machine
             4 5) 5)  "kale")) 3 " 3 kale chips bag are remaining in the machine after transaction")
  (check-equal? (machine-remaining-carrots (machine-next-state (machine-next-state (initial-machine
             4 5) 5)  "carrots")) 4 " 4 carrot sticks bag are remaining in the machine after transaction")
  (check-equal? (machine-output (machine-next-state (initial-machine 0 0) 0) "kale") "Nothing"
               "Should return nothing")
  (check-equal? (machine-output (machine-next-state (initial-machine 0 0) 0) "carrots") "Nothing"
               "Should return nothing")) 


   
              
  
               




  
 

