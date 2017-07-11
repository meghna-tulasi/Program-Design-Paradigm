;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 (require rackunit)
(require "extras.rkt")
(check-location "01" "q5.rkt")
(provide string-delete) 


;;Data Definitions :None

;; string-delete : String PosInt -> String
       ;; GIVEN: a String and an Index Number at which the
       ;;character present has to be deleted.
                 
       ;; RETURNS: Updated string with deleted character at given index number
       
;; EXAMPLES       
        ;; (string-delete "hello" 2) = "helo"
        ;;(string-delete "enterpreneur" 5) = "enterreneur"
        ;;(string-delete "good morning" 2) = "god morning"


;;DESIGN STRATEGY: Combining Functions


(define (string-delete str x)
  

  (string-append (substring str 0 x) (substring str (+ x 1) (string-length str))))
 
;;TESTS
(begin-for-test

(check-equal?
   (string-delete "aetna" 1) "atna" "Updated Substring should be 'atna.'")
(check-equal?
   (string-delete "good morning" 4) "goodmorning" "Updated Substring should be 'goodmorning'")
(check-equal?
   (string-delete "laptop" 0) "aptop" "Updated Substring should be 'aptop.'")
(check-equal?
   (string-delete "make-do" 4) "makedo" "Updated Substring should be 'makedo.'")
(check-equal?
   (string-delete " " 0) "" "Updated Substring should be ''"))





