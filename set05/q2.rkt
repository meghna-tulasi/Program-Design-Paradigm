;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;The program takes a list of(student,class) pairs
;;and produce list of class rosters,one roster for each class and has
;;atleast one student enrolled.




(require rackunit)
(require "extras.rkt")
(check-location "05" "q2.rkt")


(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 behavior-correct?
 enrollments-to-rosters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;DATA-DEFINITIONS:

;;A Student can be of any datatype and may assume that students may
;;be compared for equality with equal?

;;SetOfStudent
;;--empty
;;-(cons Class SetOfStudent)

;;TEMPLATE
;;SOS-fn : SOS-fn sos)
;; (cond
;;   [(empty? sos) ...]
;;    [else (... 
;;     (Student-fn (first sos))
;;     (SOS-fn (rest sos)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;A Class is any datatype.

;;ListOfClass
;;--empty 
;;-(cons Class ListOfClass)

;;TEMPLATE
;;LOC-fn : LOC -> ?
;;(define (LOC-fn loc)
;; (cond
;;   [(empty? loc) ...]
;;    [else (... 
;;     (Class-fn (first loc))
;;     (LOC-fn (rest loc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct roster(classname students))

;;A Roster is a
;;(make-roster Class SetOfStudent)
;;INTERPRETATION:
;;the roster represents set of students in a Class datatype classname.
;;TEMPLATE
;;roster-fn :roster-?
;;(define (roster-fn r)
;;  (...
;;    (Class-fn (roster-classname r))
;;    (SOS-fn (roster-setofstudent r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ListOfRoster
;;--empty ->list without roster
;;--(cons roster ListOfRoster) ->list representing first item is roster
;;and rest of the list.

;;TEMPLATE
;;LOR-fn : LOR -> ?
;;(define (LOR-fn lor)
;; (cond
;;   [(empty? lor) ...]
;;    [else (... 
;;     (roster-fn (first lor))
;;     (LOR-fn (rest lor)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct enrollment(student class))
;;An enrollment is a
;;(make-enrollment Student Class)
;;student of datatype "Student" enrolled in class of datatype of "Class"
;;TEMPLATE
;;enrollment-fn :enrollment-?
;;(define (enrollment-fn e)
;;  (...
;;    (Student-fn (enrollment-student e))
;;    (LOC-fn (enrollment-class e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ListOfEnrollmentAssertion
;; --empty -> list witout enrollment
;; --(cons enrollment LisOfEnrollmentAssertion)->list representing first item enrollment
;;and rest of the list.

;;TEMPLATE
;;LOE-fn:LOE->???
;;(define (LOE-fn loe)
;;  (cond
;;    [(empty? loe) ...]
;;    [else (...
;;     (enrollment-fn (first loe))
;;     (LOE-fn (rest loe)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Halting measure for all the recursive function
;;used below is the length of the list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;A SetOfX (SOX) is a
;;list of X with no duplicates
;;--empty
;;--(cons X SOX)
;; SOX-fn : SOX -> ??
;; (define (SOX-fn sox)
;;  (cond
;;    [(empty? sox) empty]
;;    [else (...
;;           (x-fn (first sox))
;;           (sox-fn (rest sox)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SUBJECT-LIST(list "PDP" "networks"))

(define PDP-LIST (list "John" "Feng" "Amy"))
(define NETWORKS-LIST (list "Amy" "Kathryn"))
(define PDP-ROSTER (make-roster "PDP" PDP-LIST))
(define NETWORK-ROSTER (make-roster "networks" NETWORKS-LIST))
(define MIX-ENROLLMENT (list (make-enrollment "John" "PDP")
                          (make-enrollment "Kathryn" "networks")
                          (make-enrollment "Feng" "PDP")
                          (make-enrollment "Amy" "PDP")
                          (make-enrollment "Amy" "networks")))
                            
(define ROSTER-LIST(list PDP-ROSTER NETWORK-ROSTER ))
(define ROSTER-DUPLICATE(list
                         (make-roster "PDP" (list "John" "John" "Feng" "Amy"))
                         (make-roster "Networks" (list "Kathryn" "Amy"))))

(define ROSTER-BADLIST1 (list (make-roster "networks" (list "Kathryn" "Amy"))))
(define ROSTER-BADLIST2 (list
 (make-roster "PDP" (list "Kathryn" "Amy"))
 (make-roster "networks" (list "John" "Feng" "Amy"))))
(define ROSTER-BADLIST3(list
 (make-roster "PDP" (list "John" "Feng" "Amy"))
 (make-roster "networks" (list "Kathryn" "Amy"))
 (make-roster "PDP" (list "John" "Feng" "Amy"))
 (make-roster "PDP" (list "John" "Feng" "Amy"))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;enrollments-to-rosters: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;;GIVEN: a set of enrollments
;;RETURNS: a correct set of class rosters for the given enrollments
;;EXAMPLES:(enrollments-to-rosters MIX-ENROLLMENT->
;;(list(make-roster "PDP" (list "John" "Feng" "Amy"))
;; (make-roster "Networks" (list "Kathryn" "Amy")))
;;STRATEGY: Using HOF on listofenrollmentassertion

(define (enrollments-to-rosters loe)
  (foldr ;;Enrollment ListOfRoster ->  ListOfRoster
         ;;GIVEN: Enrollment and list of roster
         ;;RETURNS:updated listofroster
   (lambda (e lor)
     (if (class-matches? (enrollment-class e)
                       lor)
         (add-to-rosterlist e lor)
         (cons
          (make-roster (enrollment-class e)
                       (list (enrollment-student e))) lor)))
   empty
   loe))

;;TESTS
(begin-for-test
  (check-equal?  (enrollments-to-rosters MIX-ENROLLMENT)
(list
 (make-roster "PDP" (list "John" "Feng" "Amy"))
 (make-roster "networks" (list "Kathryn" "Amy")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;class-matches? : Class ListOfRoster ->Boolean
;;GIVEN: class and list of roster
;;RETURNS: returns true if class match found in list of roster
;;EXAMPLES: (class-matches? "PDP"  (list
;;  (make-roster "PDP" (list "Feng" "Amy"))
;;  (make-roster "Networks" (list "Kathryn" "Amy")))
;; -> true
;;STRATEGY: USing HOF
(define (class-matches? class lor)
  (ormap ;;Roster -> Boolean
         ;;GIVEN: roster
         ;;RETURNS: true if classname of given roster item matches in the enrollment list
   (lambda (roster)
     (equal? class (roster-classname roster)))
   lor))

;;TESTS
(begin-for-test
  (check-equal? (class-matches? "networks" ROSTER-LIST)
true)
  (check-equal? (class-matches? "IR" ROSTER-LIST)
false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add-to-rosterlist: Enrollment ListOfRoster -> ListOfRoster
;;GIVEN:enrollment and listofroster
;;RETURNS:Updates ListofRoster
;;EXAMPLES:(add-to-rosterlist (make-enrollment "Amy" "PDP") ROSTER-LIST)->
;;(list
;; (make-roster "PDP" (list "Amy" "John" "Feng" "Amy"))
;; (make-roster "networks" (list "Amy" "Kathryn")))
;;STRATEGY:Using HOF
(define (add-to-rosterlist e lor)
  (map
   ;;Roster->Roster
   ;GIVEN:roster
   ;RETURNS: Updated roster if class match found
   (lambda (roster)
    (if(equal? (enrollment-class e) (roster-classname roster))
       (make-roster (roster-classname roster)
                    (cons (enrollment-student e) (roster-students roster)))
       roster))
   lor))
;;TESTS
(begin-for-test
  (check-equal? (add-to-rosterlist (make-enrollment "Meghna" "PDP") ROSTER-LIST)
(list
 (make-roster "PDP" (list "Meghna" "John" "Feng" "Amy"))
 (make-roster "networks" (list "Amy" "Kathryn")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRoster
;;GIVEN: a set of enrollment assertions
;;RETURN: an incorrect set of class rosters for the given enrollments.
;;The three functions should return DIFFERENT incorrect sets of class
;;rosters.
;;EXAMPLES:(enrollments-to-rosters-bad-1 MIX-ENROLLMENT)->ROSTER-BADLIST1
;;STRATEGY: Combining simple function
(define (enrollments-to-rosters-bad-1 loe)
  (new-roster (check-subject-list loe) loe))
;;TESTS:
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-1 MIX-ENROLLMENT)
                ROSTER-BADLIST1 "Incorrect roster for enrollments"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;check-subject-list: ListOfEnrollment ->ListOfClass
;GIVEN: list of enrollments
;;RETURNS: list of class
;;EXAMPLE: (check-subject-list  MIX-ENROLLMENT)->(list "networks")
;;STRATEGY: Using template of ListOfEnrollment
(define (check-subject-list loe)
  [cond
    [(empty? loe) empty]
    [(subject-not-found? (first loe) (rest loe))
     (cons (enrollment-class (first loe)) (check-subject-list (rest loe)))]
    (else (check-subject-list (rest loe)))])
;;TESTS:
(begin-for-test
  (check-equal? (check-subject-list  MIX-ENROLLMENT) (list "networks")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;subject-not-found? : Enrollment SetOfEnrollment -> Boolean
;;GIVEN: enrollment and set of enrollment
;;RETURNS: true if subject is not found
;;EXAMPLES:(subject-not-found? (make-enrollment "Amy" "PDP") MIX-ENROLLMENT)->false
;;STRATEGY: Using template of ListOfEnrollment
(define (subject-not-found? e loe)
  (cond
    [(empty? loe) true]
    [(equal? (enrollment-class e) (enrollment-class (first loe)))
     (subject-not-found? e (rest loe))]
    [else false]))
;;TESTS:
(begin-for-test
  (check-equal? (subject-not-found? (make-enrollment "Feng" "PDP")  MIX-ENROLLMENT)
false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-roster: ListOfClass ListOfEnrollment-> Roster
;;GIVEN: list of class and list of enrollment
;;RETURNS: roster
;;EXAMPLES:(new-roster SUBJECT-LIST MIX-ENROLLMENT) -> ROSTER-LIST
;;STRATEGY:Using template of ListOfEnrollemnt
(define (new-roster loc loe)
  (cond
    [(empty? loc) empty]
  [else (cons (make-roster (first loc) (new-student-list (first loc) loe))
        (new-roster (rest loc) loe))])) 
;;TESTS:
(begin-for-test
  (check-equal? (new-roster SUBJECT-LIST MIX-ENROLLMENT)
  (list
 (make-roster "PDP" (list "John" "Feng" "Amy"))
 (make-roster "networks" (list "Kathryn" "Amy")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-student-list: Class ListOfEnrollment-> ListOfStudent
;;GIVEN: class and list of enrollment
;;RETURNS: List Of Student
;;EXAMPLES:(new-student-list "PDP" MIX-ENROLLMENT)->PDP-LIST
;;STRATEGY: Using template of ListOfEnrollemnt
(define (new-student-list class loe)
  (cond
    [(empty? loe) empty]
    [(student-enrolled? class (first loe))
     (cons (enrollment-student (first loe)) (new-student-list class (rest loe)))]
    [else (new-student-list class (rest loe))]))
;;TESTS:
(begin-for-test
  (check-equal? (new-student-list "networks" MIX-ENROLLMENT)
(list "Kathryn" "Amy")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;student-enrolled? Class Enrollment -> Boolean
;;GIVEN: class and enrollment
;;RETURNS: true if stuent is enrolled in given class
;;EXAMPLES:(student-enrolled? "PDP" (make-enrollment "John" "PDP"))->true
;;STRATEGY:Combine simple function
(define (student-enrolled? class e)
  (equal? class (enrollment-class e)))
;;TESTS:
(begin-for-test
  (check-equal? (student-enrolled? "PDP" (make-enrollment "meghna" "PDP"))
true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRoster
;;GIVEN: a set of enrollment assertions
;;RETURN: an incorrect set of class rosters for the given enrollments.
;;The three functions should return DIFFERENT incorrect sets of class
;;rosters.
;;EXAMPLES:(enrollments-to-rosters-bad-2 MIX-ENROLLMENT)->ROSTER-BADLIST2
;;STRATEGY: Combining simple function
(define (enrollments-to-rosters-bad-2 loe)
  (new-roster-2 (check-subject-list-2 loe) loe))
;; TESTS:
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-2 MIX-ENROLLMENT)
(list (make-roster "networks" (list "John" "Feng" "Amy")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-roster-2 : ListOfClass ListOfEnrollment-> Roster
;;GIVEN: list of class and list of enrollment
;;RETURNS: roster
;;EXAMPLES:(new-roster SUBJECT-LIST MIX-ENROLLMENT) -> ROSTER-LIST
;;STRATEGY:Using template of ListOfEnrollemnt
(define (new-roster-2 loc loe)
  (cond
    [(empty? loc) empty]
  [else (cons (make-roster (first loc)(new-student-list-2 (first loc)  loe))
        (new-roster-2 (rest loc) loe))]))
;;TESTS:

(begin-for-test
  (check-equal? (new-roster-2 SUBJECT-LIST MIX-ENROLLMENT) ROSTER-BADLIST2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-student-list-2: Class ListOfEnrollment-> ListOfStudent
;;GIVEN: class and list of enrollment
;;RETURNS: List Of Student
;;EXAMPLES:(new-student-list-2 "PDP" MIX-ENROLLMENT)->(list "Kathryn" "Amy")
;;STRATEGY: Using template of ListOfEnrollemnt
(define (new-student-list-2 class loe)
  (cond
    [(empty? loe) empty]
    [(subject-enrolled? class (first loe))
     (cons (enrollment-student (first loe)) (new-student-list-2 class (rest loe)))]
    [else (new-student-list-2 class (rest loe))]))
;;TESTS:

(begin-for-test
  (check-equal?(new-student-list-2 "PDP" MIX-ENROLLMENT)(list "Kathryn" "Amy")))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;subject-enrolled?: Class Enrollment -> Boolean
;;GIVEN: class and enrollment
;;RETURNS: true if both classes dont match
;;EXAMPLES:(subject-enrolled? "PDP" (make-enrollment "John" "PDP"))->false
;;STRATEGY:Combining simple functions
(define (subject-enrolled? class e)
   (not (equal? class (enrollment-class e))))
;;;;TESTS:

(begin-for-test
  (check-equal? (subject-enrolled? "PDP" (make-enrollment "John" "PDP"))false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-subject-list-2: ListOfEnrollment-> ListOfClass
;;GIVEN: list of eenrollment
;;RETURNS: list of class for an enrollment
;;EXAMPLE: (check-subject-list-2 MIX-ENROLLMENT)
;;->(list "networks")
;;STRATEGY: Using template of loe
(define (check-subject-list-2 loe)
  (cond
    [(empty? loe) empty]
    [(class-not-found? (first loe) (rest loe))
    (cons (enrollment-class (first loe)) (check-subject-list-2 (rest loe)))]
    [else (check-subject-list-2 (rest loe))]))

;;TESTS:
(begin-for-test
  (check-equal? (check-subject-list-2 MIX-ENROLLMENT)
(list "networks")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;class-not-found?: Enrollment ListOfEnrollment-> Boolean
;;GIVEN:enrollment and list of eenrollment
;;RETURNS: true if class not found
;;EXAMPLE: (class-not-found? (make-enrollment "Amy" "PDP") MIX-ENROLLMENT)->false
;;STRATEGY: Using template of loe
(define (class-not-found? e loe)
  (cond
    [(empty? loe) true]
    [(different-class? e (first loe)) true]
    [else false]))
;;TESTS:
(begin-for-test
  (check-equal? (class-not-found? (make-enrollment "Amy" "PDP")
                                  MIX-ENROLLMENT)false))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;different-class?: Enrollment Enrollment -> Boolean
;;GIVEN: two enrollments
;;RETURNS: true if both enrollments do not match
;;EXAMPLE: (different-class?  (make-enrollment "Amy" "PDP") (make-enrollment "Amy" "PDP"))->false
(define (different-class? e1 e2)
   (not (equal? (enrollment-class e1) (enrollment-class e1))))
;;TESTS:
(begin-for-test
 (check-equal? (different-class?  (make-enrollment "Amy" "PDP") (make-enrollment "Amy" "PDP"))false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRoster
;;GIVEN: a set of enrollment assertions
;;RETURN: an incorrect set of class rosters for the given enrollments.
;;The three functions should return DIFFERENT incorrect sets of class
;;rosters.
;;EXAMPLES:(enrollments-to-rosters-bad-3 MIX-ENROLLMENT)->ROSTER-BADLIST3
;;STRATEGY: Combining simple function
(define (enrollments-to-rosters-bad-3 loe)
  (new-roster (check-subject-list-3 loe) loe))
;;TESTS:
(begin-for-test
 (check-equal? (enrollments-to-rosters-bad-3 MIX-ENROLLMENT)ROSTER-BADLIST3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-subject-list-3: ListOfEnrollment-> ListOfClass
;;GIVEN: list of enrollment
;;RETURNS: list of class for an enrollment
;;EXAMPLE: (check-subject-list-3 MIX-ENROLLMENT)
;;->(list "PDP" "networks" "PDP" "PDP")
;;STRATEGY: Using template of loe
(define (check-subject-list-3 loe)
  (cond
    [(empty? loe) empty]
    [(subject-not-found-2? (first loe) (rest loe))
     (cons (fetch-subject (first loe)) (check-subject-list-3 (rest loe)))]
    [else (check-subject-list-3 (rest loe))]))

(define (fetch-subject e)
  (enrollment-class e))
;;TESTS:
(begin-for-test
 (check-equal? (check-subject-list-3 MIX-ENROLLMENT)
(list "PDP" "networks" "PDP" "PDP")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;subject-not-found-2? : Enrollment SetOfEnrollment -> Boolean
;;GIVEN: enrollment and set of enrollment
;;RETURNS: true if subject is not found
;;EXAMPLES:(subject-not-found-2? (make-enrollment "Amy" "PDP") MIX-ENROLLMENT)->true
;;STRATEGY: Using template of ListOfEnrollment
(define (subject-not-found-2? e loe)
  (cond
    [(empty? loe) false]
    [(different-class? e (first loe)) (subject-not-found-2? e (rest loe))]
    [else true]))                                       
;;TESTS: 
(begin-for-test
  (check-equal? (subject-not-found-2? (make-enrollment "Amy" "PDP") MIX-ENROLLMENT)
true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
;;GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
;;RETURNS: true iff the output of soln-fn on se is an example of correct
;;behavior by a ProposedSolution.
;;EXAMPLE: (behavior-correct? enrollments-to-rosters-bad-1 MIX-ENROLLMENT)->true
;;STRATEGY: Combining simple function
(define (behavior-correct? solution loe)
  (check-behavior? (solution loe) loe))

;;TESTS:
(begin-for-test
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-1
                                   MIX-ENROLLMENT)true)
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-2
                                   MIX-ENROLLMENT)true)
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-3
                                   MIX-ENROLLMENT)false))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-behavior? :ListOfRoster ListOfEnrollment -> Boolean
;;GIVEN: list of roster and enrollment
;;RETURNS: true if roster has correct enrollment according to subject and student
;;EXAMPLES: (check-behavior? ROSTER-LIST MIX-ENROLLMENT)->false
;;STRATEGY: Combining simple function
(define (check-behavior? lor loe)
  (and 
   (check-subject-behavior? lor loe)
   (check-student-behavior? lor)
   (correct-enrollment? lor loe)))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-subject-behavior? : ListOfRoster ListOfEnrollment-> Boolean
;;GIVEN: list of roster and enrollment
;;RETURNS: true if
;;EXAMPLES: (check-subject-behavior? ROSTER-LIST MIX-ENROLLMENT)->true
;;STRATEGY: Combining simple function
(define (check-subject-behavior? lor loe)
  (check-offered-subject? lor (check-subject-list loe)))

;;TESTS:
(begin-for-test
  (check-equal? (check-subject-behavior? ROSTER-LIST MIX-ENROLLMENT)true))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;check-offered-subject? : ListOfRoster ListOfClass-> Boolean
;;GIVEN: list of roster and class
;;RETURNS: true if
;;EXAMPLES: (check-offered-subject? ROSTER-LIST (list "PDP" "networks"))->true
;;STRATEGY: Using template of ListOfRoster
(define (check-offered-subject? lor loc)
  (cond
    [(empty? loc) true]
    [(is-subject-offered? lor (first loc)) (check-offered-subject? lor (rest loc))]
    [else false]))
;;TESTS:
(begin-for-test
  (check-equal? (check-offered-subject? ROSTER-LIST (list "PDP" "networks"))
#true)
  (check-equal? (check-offered-subject? ROSTER-LIST (list "IR" "networks"))
#false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;is-subject-offered? : ListOfRoster Class-> Boolean
;;GIVEN: list of roster and class
;;RETURNS: true if subject is in roster
;;EXAMPLES: (is-subject-offered? ROSTER-LIST "PDP")->true
;;STRATEGY: Using template of ListOfRoster
(define (is-subject-offered? lor class)
  (cond
    [(empty? lor) false]
    [(equal? (roster-classname (first lor)) class) true]
    [else (is-subject-offered? (rest lor) class)]))
;;TESTS:
(begin-for-test
  (check-equal?  (is-subject-offered? ROSTER-LIST "PDP")
#true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-student-behavior? : ListOfRoster -> Boolean
;;GIVEN: list of roster 
;;RETURNS: true if students are in roster
;;EXAMPLES: (check-student-behavior? ROSTER-LIST)->true
;;STRATEGY: Using template of ListOfRoster

(define (check-student-behavior? lor)
  (cond
    [(empty? lor) true]
    [(duplicate-entry? (first lor)) (check-student-behavior? (rest lor))]
    [else false]))
;;TESTS:
(begin-for-test
  (check-equal? (check-student-behavior? ROSTER-LIST) true)
  (check-equal? (check-student-behavior? ROSTER-DUPLICATE)false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;duplicate-entry? :Roster -> Boolean
;;GIVEN: roster 
;;RETURNS: true if no duplicate entry present
;;EXAMPLES: (duplicate-entry? (make-roster "PDP" (list "John")))->true
;;STRATEGY: Combining simple functions
(define (duplicate-entry? lor)
  (check-duplicate? (roster-students lor)))

;;TESTS:
(begin-for-test
  (check-equal? (duplicate-entry? (make-roster "PDP" (list "John")))true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-duplicate? : ListOfStudent -> Boolean
;;GIVEN: list of student
;;RETURNS: true if no duplicate entry of student
;;EXAMPLE:(check-duplicate? (list "john" "amy"))->true
;;STRATEGY:  Using template of SetOfStudent
(define (check-duplicate? new-student-list-2)
    (cond 
      [(empty? new-student-list-2) true]
      [(student-exists? (first new-student-list-2) (rest new-student-list-2))false]
      [else (check-duplicate? (rest new-student-list-2))]))
;;TESTS:
(begin-for-test
  (check-equal? (check-duplicate? (list "john" "amy")) true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;student-exists?: Student SetOfStudent->Boolean
;;GIVEN: student and list of student
;;RETURNS: true if entry exists of the student
;;EXAMPLES:(student-exists? "John" (list "John" "Feng" "Amy"))->true
;;STRATEGY: Using template of SetOfStudent
(define (student-exists? s sos)
  (cond
    [(empty? sos) false]
    [(equal? s (first sos)) true]
    [else (student-exists? s (rest sos))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;correct-enrollment?: ListOfRoster ListOfEnrollment -> Boolean
;;GIVEN: List of roster and enrollment
;;RETURNS:true if the enrolled student element for a class exists
;;EXAMPLES:(correct-enrollment? ROSTER-LIST MIX-ENROLLMENT)->false
;;STRATEGY:Using template of ListOfRoster

(define (correct-enrollment? lor loe)
  (cond
    [(empty? lor) true]
    [(enrollment-correct? (first lor) (first loe))
     (correct-enrollment? (rest lor) loe)]
    [else false]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;enrollment-correct?: Class Enrollment -> Boolean
;;GIVEN: Class and Enrollment
;;RETURNS: true if student in the roster class matches with enrolled class
;;EXAMPLES:(enrollment-correct? (make-roster "PDP" "John") (make-enrollment "PDP" "John"))->true
;;STRATEGY: Combining simple functions
(define (enrollment-correct? class e)
(if (equal? (roster-classname class) (enrollment-class e))
    (if (equal? (first (roster-students class)) e) true false) true))
  
;;TESTS:
(begin-for-test
  (check-equal? (enrollment-correct? (make-roster "PDP" "John") (make-enrollment "PDP" "John"))
true)
  (check-equal? (enrollment-correct? (make-roster "IR" "John") (make-enrollment "PDP" "John"))
true))

