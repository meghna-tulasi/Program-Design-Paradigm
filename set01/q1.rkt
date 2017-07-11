;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q1.rkt")
 
(provide distance-to-origin) 


;;Data Definitions :None

;; distance-to-origin : Real Real -> PosReal
       ;; GIVEN: x and y coordinates of a point
       ;; RETURNS: Distance[Positive Real Number] between Origin(0,0) and the given equivalent point.
       
;; EXAMPLES       
;; (distance-to-origin  12 5) = 13

;; (distance-to-origin -3 4) = 5
;; (distance-to-origin  1 -1) = 1.41
;; (distance-to-origin  -6 -8) = 10

;;DESIGN STRATEGY: Composing a Function

(define (distance-to-origin x y)
  (sqrt (+ (* x x) (* y y))))


;; TESTS

(begin-for-test
 (check-equal?
 (distance-to-origin 12 5) 13 "Distance between point(12,5) and origin(0,0) should be 13.")
 
 (check-equal?
 (distance-to-origin -1 -1) #i1.4142135623730951 "Distance between point(-1,-1) and origin(0,0) should be 1.41.")
 
 (check-equal?
 (distance-to-origin -26 30) #i39.698866482558415 "Distance between point(-26,30) and origin(0,0) should be 39.69.")
 
 (check-equal?
 (distance-to-origin 10.3 -3) #i10.72800074571213  "Distance between point(10.3,-3) and origin(0,0) should be #i10.72800074571213.")
 
 (check-equal?
 (distance-to-origin 0 0) 0 "Distance between point(0,0) and origin(0,0) should be 0.")
 
 (check-equal?
 (distance-to-origin 0 -1.2) 1.2 "Distance between point(0,-1.2) and origin(0,0) should be 1.2.")
 
 (check-equal?
 (distance-to-origin 0 1) 1 "Distance between point(0,1) and origin(0,0) should be 1.")
 
 (check-equal?
 (distance-to-origin 3 0) 3 "Distance between point(3,0) and origin(0,0) should be 3.")
 
 (check-equal?
 (distance-to-origin -3 0) 3 "Distance between point(-3,0) and origin(0,0) should be 3"))
 
 

