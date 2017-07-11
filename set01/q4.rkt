;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q4.rkt")

(provide string-insert) 
 

;;Data Definitions :None

;; string-insert : String PosInt -> String
       ;; GIVEN: a String and an Index Number at which "_" has to be inserted
                 
       ;; RETURNS: Updated string with "_" at given index number
       
;; EXAMPLES       
        ;; (string-insert "hello" 2) = "he_llo"
        ;;(string-insert "enterpreneur" 5) = "enter_preneur"
        ;;(string-insert "good morning" 2) = "go_od morning"


;;DESIGN STRATEGY: Composing a Function


(define (string-insert str x)
  

  (string-append (string-append (substring str 0 x) "_") (substring str x (string-length str))))
 
;;TESTS
(begin-for-test

(check-equal?
   (string-insert "aetna" 1) "a_etna" "Updated Substring should be 'a_etna.'")
(check-equal?
   (string-insert "good morning" 4) "good_ morning" "Updated Substring should be 'good_ morning.'")
(check-equal?
   (string-insert "water" 0) "_water" "Updated Substring should be '_water.'") 
(check-equal?
   (string-insert "water" 5) "water_" "Updated Substring should be 'water_.'") 
(check-equal?
   (string-insert "wait" 2) "wa_it" "Updated Substring should be 'wa_it.'"))
