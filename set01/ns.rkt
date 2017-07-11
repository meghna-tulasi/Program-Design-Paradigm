;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide next-state
         accepting-state?
         initial-machine)

(provide error-state?)


    
  
    
  
   (define (next-state s mi)
  (cond
    
    [(or (and (string=? "A" s)(string=? "x" mi)) 
         
         (and (string=? "A" s)(string=? "q" mi))) "A"]
    

    [(or (and (string=? "A" s)(string=? "d" mi))

         (and (string=? "C" s)(string=? "d" mi))

         (and (string=? "D" s)(string=? "d" mi))

         (and (string=? "B" s)(string=? "e" mi))

         (and (string=? "B" s)(string=? "f" mi))) "B"]
    

    [(or (and (string=? "A" s)(string=? "u" mi))

         (and (string=? "C" s)(string=? "u" mi))) "C"]
    

    [(or (and (string=? "C" s)(string=? "q" mi))

         (and (string=? "C" s)(string=? "x" mi))

         (and (string=? "A" s)(string=? "e" mi))

         (and (string=? "A" s)(string=? "f" mi))

         (and (string=? "D" s)(string=? "q" mi))

         (and (string=? "D" s)(string=? "x" mi))

         (and (string=? "D" s)(string=? "u" mi))

         (and (string=? "D" s)(string=? "e" mi))

         (and (string=? "D" s)(string=? "f" mi))

         (and (string=? "C" s)(string=? "f" mi))

         (and (string=? "C" s)(string=? "e" mi))

         (and (string=? "B" s)(string=? "q" mi))

         (and (string=? "B" s)(string=? "x" mi))

         (and (string=? "B" s)(string=? "u" mi))

         (and (string=? "B" s)(string=? "a" mi))

         (and (string=? "B" s)(string=? "b" mi))) "ER"]
         

    
    [(or (and (string=? "A" s)(string=? "a" mi)) 
         
         (and (string=? "A" s)(string=? "b" mi))

         (and (string=? "D" s)(string=? "a" mi))

         (and (string=? "D" s)(string=? "b" mi))

         (and (string=? "C" s)(string=? "a" mi))

         (and (string=? "C" s)(string=? "b" mi))) "D"]))




(begin-for-test
  (check-equal? (next-state "A" "q") "A")
  (check-equal? (next-state "C" "a") "D")
  (check-equal? (next-state "D" "b") "D")
  (check-equal? (next-state "B" "e") "B"))



(define (accepting-state? as)
  (cond
    [(string=? "A" as) false]
    [(string=? "B" as) true]
    [(string=? "C" as) false]
    [(string=? "D" as) false]))

(begin-for-test
  (check-equal? (accepting-state? "A") #false)
  (check-equal? (accepting-state? "B") #true)
(check-equal? (accepting-state? "C") #false)
(check-equal? (accepting-state? "D") #false))




(define (error-state? es)
 (cond
   
        [(equal? (next-state es "q") "ER") "ER"]
        [(equal? (next-state es "x") "ER") "ER"]
         [(equal? (next-state es "u") "ER") "ER"]
          [(equal? (next-state es "b") "ER") "ER"]
           [(equal? (next-state es "e") "ER") "ER"]
            [(equal? (next-state es "f") "ER") "ER"]
             [(equal? (next-state es "a") "ER") "ER"]))
        

(begin-for-test
  (check-equal? (error-state? "C") "ER" )
  (check-equal? (error-state? "A") "ER" )
  (check-equal? (error-state? "B") "ER" )
  (check-equal? (error-state? "D") "ER" ))
  

(define (initial-machine x y)
(and (if (positive? x) true false) (if(positive? y) true false)))
     



   