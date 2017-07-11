;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q3.rkt")
(require 2htdp/image)
(define cir (circle 10 "solid" "red"))
(define rec (rectangle 10 20 "solid" "blue"))
(define sta (star 6 "solid" "yellow"))


(provide image-area)


;;Data Definitions:None


 ;; image-area : ImageName -> PosInt
 ;; GIVEN:  Image Name
 ;; RETURNS: Area of the Image in pixels
       
;;Examples:
  ;;(image-area cir) = 100
  ;;(image-area rec) = 42
  ;;(image-area sta) = 64
  
  
;;DESIGN STRATEGY: Composing a Function

(define (image-area imagename)
  
  (* (image-width imagename) (image-height imagename)))


;; TESTS

(begin-for-test
 (check-equal?
 (image-area cir) 400 "Area of the given image is 400.")
  (check-equal?
 (image-area rec) 200 "Area of the given image is 200.")
  (check-equal?
 (image-area sta) 90 "Area of the given image is 90."))
  
 
 