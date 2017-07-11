;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;The solution aims to seperate the mixed slips of Professor Fellesein and Shivers
;;Also, to implement with higher order functions.




(require rackunit)
(require "extras.rkt")
(check-location "05" "q1.rkt")

(provide
 felleisen-roster
 shivers-roster
 possible-roster?
 acceptable-felleisen-answer?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;DATA-DEFINITIONS:

(define-struct slip (color name1 name2))
;;A Slip is a
;;(make-slip Color String String)
;;Color is one of the yellow or blue slips of Professor Fellesein and Shivers repectively
;;name1 is the first/last name of student 
;;name2 is the first/last name of student 

;;Template
;; slip-fn: Slip -> ??
#;(define (slip-fn s)
    (slip-color s)
    (slip-name1 s)
    (slip-name2 s))

;;A Color is one of:
;;--"yellow"
;;--"blue"

;;Template:
;;color-fn : Color -> ?
#;(define (color-fn c)
    (cond
      [(string=? c "yellow")]
      [(string=? c "blue")]))

;;ListOfSlip
;;-empty
;;--(cons Slip LOS)
;;LOS is the list of slips 
;;Template:
;;los-fn : LOS -> ?
;;HALTING MEASURE: last item of list
#;(define (los-fn los)
    (cond
      [(empty? los)...]
      [else(..
            (slip-fn (first-los))
            (los-fn (rest los)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANT-DEFINITIONS:

(define YELLOW "yellow")
(define BLUE "blue")

(define YELLOW-SLIP1 (make-slip "yellow" "Xi" "Wang"))
(define YELLOW-SLIP2 (make-slip "yellow" "Wang" "Xi"))
(define YELLOW-SLIP3(make-slip "yellow" "Shriram" "K."))
(define YELLOW-SLIP4 (make-slip "yellow" "Meghna" "T"))
(define EMPTY-LIST (list '()))

(define BLUE-SLIP1 (make-slip "blue" "Jones" "Tom"))
(define BLUE-SLIP2 (make-slip "blue" "Tom" "Jones"))
(define BLUE-SLIP3 (make-slip "blue" "Meghna" "T"))
(define BLUE-SLIP4 (make-slip "blue" "John" "S"))

(define YELLOW-DUPLICATE-LIST (list YELLOW-SLIP1 YELLOW-SLIP2))
(define BLUE-DUPLICATE-LIST (list BLUE-SLIP1 BLUE-SLIP2))

(define YELLOW-NONDUPLICATE-LIST (list YELLOW-SLIP3 YELLOW-SLIP4))
(define BLUE-NONDUPLICATE-LIST (list BLUE-SLIP3 BLUE-SLIP4))

(define MIX-YELLOWLIST (append YELLOW-DUPLICATE-LIST YELLOW-NONDUPLICATE-LIST))
(define MIX-BLUELIST (append BLUE-DUPLICATE-LIST BLUE-NONDUPLICATE-LIST))

(define MIX-DUPLICATE(append YELLOW-DUPLICATE-LIST BLUE-DUPLICATE-LIST))
(define MIX-NONDUPLICATE(append YELLOW-NONDUPLICATE-LIST BLUE-NONDUPLICATE-LIST))

(define MIX-LIST (append YELLOW-DUPLICATE-LIST BLUE-DUPLICATE-LIST
                         YELLOW-NONDUPLICATE-LIST BLUE-NONDUPLICATE-LIST))
(define FELLEISEN-LIST1 (cons YELLOW-SLIP1 YELLOW-NONDUPLICATE-LIST))
(define FELLEISEN-LIST2 (cons YELLOW-SLIP2 YELLOW-NONDUPLICATE-LIST))

(define SHIVERS-LIST1 (cons BLUE-SLIP1 BLUE-NONDUPLICATE-LIST))
(define SHIVERS-LIST2 (cons BLUE-SLIP2 BLUE-NONDUPLICATE-LIST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;felleisen-roster : ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Felleisen's class, without duplication.
;;EXAMPLES: (felleisen-roster FELLEISEN-LIST1)->
;(list
; (make-slip "yellow" "Xi" "Wang")
; (make-slip "yellow" "Shriram" "K.")
; (make-slip "yellow" "Meghna" "T"))
;;STRATEGY: Combine simple function

(define (felleisen-roster los)
  (check-duplicate
   (check-list-color los YELLOW)))

;;TESTS:
(begin-for-test
  (check-equal? (felleisen-roster FELLEISEN-LIST1)
                (list
                 (make-slip "yellow" "Xi" "Wang")
                 (make-slip "yellow" "Shriram" "K.")
                 (make-slip "yellow" "Meghna" "T"))
                "Function Should return three slips of yellow")
  (check-equal? (felleisen-roster FELLEISEN-LIST2)
                (list
                 (make-slip "yellow" "Wang" "Xi")
                 (make-slip "yellow" "Shriram" "K.")
                 (make-slip "yellow" "Meghna" "T"))
                "Function Should return three slips of yellow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shivers-roster : ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Shivers's class, without duplication.
;;EXAMPLES: (shivers-roster (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K."))
;;            -> (list ((make-slip "blue" "JOnes" "Tom")))

;;STRATEGY: Combine simple function
(define (shivers-roster los)
  (check-duplicate (check-list-color los BLUE)))

;; TESTS:
(begin-for-test
  (check-equal? (shivers-roster SHIVERS-LIST2)
                (list
                 (make-slip "blue" "Tom" "Jones")
                 (make-slip "blue" "Meghna" "T")
                 (make-slip "blue" "John" "S")))
  
  (check-equal? (shivers-roster SHIVERS-LIST1)
                (list
                 (make-slip "blue" "Jones" "Tom")
                 (make-slip "blue" "Meghna" "T")
                 (make-slip "blue" "John" "S"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-list-color:ListOfSlip Color -> ListOfSlip
;;GIVEN: ListOfSlip and color(YELLOW/BLUE)
;;RETURNS: the list of slip which contains slips having given color
;;EXAMPLES: (check-list-color MIX-LIST YELLOW)->
;(list
; (make-slip "yellow" "Xi" "Wang")
; (make-slip "yellow" "Wang" "Xi")
; (make-slip "yellow" "Shriram" "K.")
; (make-slip "yellow" "Meghna" "T"))
;;                            
;;STRATEGY:Using HOF on los
(define (check-list-color los c)
  (filter ;Slip Color -> Boolean
   ;GIVEN: slip and color of the slip
   ;RETURNS: true if given slip color matches with given color
   (lambda (s) (string=? (slip-color s) c))
   los))
;; TESTS:
(begin-for-test
  (check-equal? (check-list-color MIX-LIST BLUE)
                (list
                 (make-slip "blue" "Jones" "Tom")
                 (make-slip "blue" "Tom" "Jones")
                 (make-slip "blue" "Meghna" "T")
                 (make-slip "blue" "John" "S"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-duplicate : ListOfSlip ->  ListOfSlip 
;;GIVEN:  list of slip
;;RETURNS: listofslip after removing duplicate names
;;EXAMPLES: (check-duplicate? 
;;(list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K."))->
;;(list
;; (make-slip "blue" "JOnes" "Tom")
;; (make-slip "yellow" "Xi" "Wang")
;; (make-slip "yellow" "Shriram" "K."))
;;STRATEGY: Using HOF on los
(define (check-duplicate los)
  (foldr
   ;slip listofslip ->listofslip
   ;GIVEN: slip and listof slip to find duplicate
   ;RETURNS: listof slip without any duplicates
   (lambda (s los) (if (duplicate? s los) los
                       (cons s los))) empty los))

;;TESTS:
(begin-for-test
  (check-equal? (check-duplicate MIX-DUPLICATE)
                (list (make-slip "yellow" "Wang" "Xi") (make-slip "blue" "Tom" "Jones")))
  (check-equal? (check-duplicate YELLOW-DUPLICATE-LIST)
                (list (make-slip "yellow" "Wang" "Xi"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;duplicate? : Slip ListOfSlip -> Boolean
;;GIVEN: slip and listof slip to find duplicate
;;RETURNS: true if duplicate found
;;STRATEGY: Using HOF on los
(define (duplicate? s los)
  (ormap
   ;RETURNS:true if name on the slip matches
   (lambda (sname1) (name-matches? s sname1)) los))
;;TESTS:
(begin-for-test
  (check-equal? (duplicate? BLUE-SLIP2 BLUE-DUPLICATE-LIST)
                #true)
  (check-equal? (duplicate? BLUE-SLIP2 YELLOW-DUPLICATE-LIST)
                #false)
  (check-equal? (duplicate? YELLOW-SLIP1 YELLOW-NONDUPLICATE-LIST)
                #false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;name-matches?: Slip slip ->Boolean
;;GIVEN: slips to compare names
;;RETURNS:true if name on the slip matches
;;EXAMPLES: (name-matches? (make-slip "yellow" "Wang" "Xi")
;;                         (make-slip "blue" "JOnes" "Tom"))->false
;;STRATEGY:Using template for slip
(define (name-matches? sname1 sname2)
  (or
   (and (string=? (slip-name1 sname1) (slip-name1 sname2))
        (string=? (slip-name2 sname1) (slip-name2 sname2)))
   (and (string=? (slip-name1 sname1) (slip-name2 sname2))
        (string=? (slip-name2 sname1) (slip-name1 sname2)))))

;;TESTS:
(begin-for-test
  (check-equal? (name-matches? (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Wang" "Tom"))
                false "Both the slips have names of different students"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;possible-roster? : ListOfSlip -> Boolean
;;GIVEN; list of slips
;;RETURNS: true iff all slips in the list are the same color,
;;and no student is represented twice.
;;EXAMPLE: (possible-roster? (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K.")) -> false
;;STRATEGY:Combine simple function
(define (possible-roster? los)
  (and (is-color-same? (first los) (rest los))
       (not (duplicate? (first los) (rest los)))))

;;TESTS:
(begin-for-test
  (check-equal? (possible-roster? YELLOW-NONDUPLICATE-LIST)
                true "List contains all yellow and no duplicate slips hence returns true")
  (check-equal? (possible-roster? BLUE-DUPLICATE-LIST)
                false "List contains all blue and duplicate slips hence returns false")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;is-color-same? : Slip ListOfSlip -> Boolean
;;GIVEN;  slip listofslip
;;RETURNS: true iff all slips in the list are the same color
;;STRATEGY:Using HOF on los
(define (is-color-same? s los)
  (ormap
   ;RETURNS: true iff both the slips are of same color
   (lambda (s1) (color-same? s s1)) los))
;;TESTS:
(begin-for-test
  (check-equal? (is-color-same? YELLOW-SLIP1 YELLOW-NONDUPLICATE-LIST) true)
  (check-equal? (is-color-same? YELLOW-SLIP1 BLUE-DUPLICATE-LIST) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;color-same?: Slip Slip -> Boolean
;;GIVEN;  slip slip
;;RETURNS: true iff both the slips are of same color
;;STRATEGY:Using template of slip
(define (color-same? s1 s2)
  (string=? (slip-color s1) (slip-color s2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;;acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;;GIVEN: two lists of slips, lst1 and lst2
;;RETURNS: true iff every student on a yellow slip in lst1 appears once and only once in lst2.
;;EXAMPLES: (acceptable-felleisen-answer? MIX-LIST YELLOW-NONDUPLICATE-LIST)->false 
;;STRATEGY: Using ListOfSlip template
;;HALTING MEASURE: last item of list

(define (acceptable-felleisen-answer? los1 los2)
  (cond
    [else (if (possible-roster? los2) 
              (check-answer (felleisen-roster los1) los2) false)]))


;;TESTS:
(begin-for-test
  (check-equal? (acceptable-felleisen-answer? MIX-LIST YELLOW-NONDUPLICATE-LIST) 
                false " mix list contains duplicate elements")
  (check-equal? (acceptable-felleisen-answer? MIX-NONDUPLICATE YELLOW-NONDUPLICATE-LIST) 
                true "the list1 contains nonduplicate andsimilar color elements")
  (check-equal? (acceptable-felleisen-answer?  EMPTY-LIST YELLOW-DUPLICATE-LIST)
                false "the first list is empty")
  (check-equal? (acceptable-felleisen-answer? YELLOW-DUPLICATE-LIST  EMPTY-LIST)
                false)
  (check-equal? (acceptable-felleisen-answer? EMPTY-LIST MIX-LIST)
                false)
  (check-equal? (acceptable-felleisen-answer? MIX-LIST FELLEISEN-LIST1)
                true)
  (check-equal? (acceptable-felleisen-answer? EMPTY-LIST EMPTY-LIST)
                false)
  (check-equal? (acceptable-felleisen-answer? MIX-LIST FELLEISEN-LIST1)
                true)
  (check-equal? (acceptable-felleisen-answer? MIX-LIST FELLEISEN-LIST2)
                true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-answer : ListOfSlip ListOfSlip -> Boolean
;;GIVEN:  lists of slip
;;RETURNS: true only if slip exists once in the list2 and is yellow
;;EXAMPLES:(check-answer MIX-LIST FELLEISEN-LIST1)->false
;;STRATEGY:Using HOF andmap

(define (check-answer los1 los2)
  (andmap
   ;Slip ListOfSlip ->Boolean
   ;RETURNS: true only if slip exists once in the list2 and is yellow
   (lambda (s) (check-requirements? s los2)) los1))

;;TESTS:
(begin-for-test
  (check-equal? (check-answer MIX-LIST FELLEISEN-LIST1)
                false)
  (check-equal? (check-answer YELLOW-NONDUPLICATE-LIST FELLEISEN-LIST1)
                true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-requirements? : Slip ListOfSlip -> Boolean
;;GIVEN: Slip list of slip
;;RETURNS: true only if slip exists once in the list2 and is yellow
;;EXAMPLES:(check-requirements? YELLOW-SLIP1 YELLOW-NONDUPLICATE-LIST)->false
;;STRATEGY:Using HOF ormap
(define (check-requirements? s los)
  (ormap
   ;RETURNS: true only if name matches 
   (lambda (sname) (and (name-matches? s sname)
                        (color-same? s sname))) los))
