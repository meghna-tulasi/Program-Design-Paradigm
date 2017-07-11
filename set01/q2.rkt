;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q2.rkt")

(provide string-last) 
 

;;Data Definitions :None

;; string-last : String PosInt -> String
       ;; GIVEN: a String and an Index Number from where the remaining
                 ;;string neeeds to be extracted.
       ;; RETURNS: String from the given index number till the end of the same string
       
;; EXAMPLES       
        ;; (string-last "hello" 2) = "llo"
        ;;(string-last "enterpreneur" 5) = "preneur"
        ;;(string-last "good morning" 2) = "od morning"


;;DESIGN STRATEGY: Composing a Function


(define (string-last str x)
  
  (substring str x (string-length str)))

;;TESTS

(begin-for-test
  
  (check-equal?
   (string-last "aetna" 1) "etna" "Last Substring should be 'etna.'")

(check-equal?
   (string-last "lakeside_electric" 4) "side_electric" "Last Substring should be 'side_electric'")
(check-equal?
 (string-last "lakeside electric" 8)  " electric" "Last Substring should be ' electric'"))


