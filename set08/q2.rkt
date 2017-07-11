;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(check-location "08" "q2.rkt")


(provide
 is-null-derivable?
 make-pos
 make-neg
 make-clause)



;;DATA-DEFINITIONS:


;; A Variable is a Racket Symbol.
;;
;; A Literal is one of
;; -- (make-pos Variable)  Interp: a literal containing the variable
;; -- (make-neg Variable)  Interp: a literal containing the negation of
;;                                the variable
;;
;; TEMPLATE:
;; literal-fn : LITERAL -> ?
#;
(define (literal-fn l)
  (cond
    [(pos? l) (... (pos-name l))]
    [(neg? l) (... (neg-name l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct pos(variable))
;;A Pos is a
;;(make-pos Variable)
;;Interp: a literal containing the variable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct neg(variable))
;;A Neg is a
;;(make-neg Variable)
;;Interp: a literal containing the negation of  the variable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;SetOfLiteral is one of the following:
;;-- empty
;;-- (set-cons Literal SOL)

;; TEMPLATE:
;; sol-fn : SOL -> ?
#;
(define (sol-fn sol)
  (cond
    [(empty? sol) ...]
    [else
     (literal-fn (first sol))
     (sol-fn (rest sol))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct clauses(sol))
;;A Clauses is
;;(make-clauses SetOfLiteral)
;;SetOfLiteral is the set of literals.
;; A Clause is a SetOfLiteral

;;TEMPLATE:
;; clauses-fn : clauses -> ?

#;
(define (clauses-fn clauses)
  (sol-fn clauses))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ListOfClause is
;;--empty
;;--(cons clause LOC)

;;TEMPLATE:
;;loc-fn : LOC-?
;(define (loc-fn loc)
;  (cond
;    [empty? loc)...]
;    [else (...
;           (clause-fn (first loc))
;           (loc-fn (rest loc))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTING-VARIABLES:

(define sample
  (list
   (make-clauses (list (make-pos'a)))
   (make-clauses (list (make-neg'a)))))



(define sample-1
  (list
   (make-clauses (list (make-pos 'a) (make-neg 'b) (make-pos 'c) ))
   (make-clauses (list (make-neg 'a)  (make-pos 'c) ))
   (make-clauses (list  (make-pos 'b)))
   (make-clauses (list (make-neg 'c)))))

(define sample-2
  (list
   (make-clauses (list (make-pos 'a) (make-neg 'b) (make-pos 'c) ))
   (make-clauses (list (make-neg 'a)  (make-neg 'c)))))



(define sample-3
  (list
   (make-clauses (list (make-pos 'a) (make-pos 'c) (make-neg 'a) (make-neg 'c)))
   (make-clauses (list (make-pos 'b) (make-neg 'a)))
   (make-clauses (list (make-pos 'c)))
   (make-clauses (list (make-pos 'a)))
   (make-clauses (list (make-neg 'c)))))

(define sample-4
  (list
   (make-clauses (list (make-pos 'a) (make-pos 'b)))
   (make-clauses (list (make-neg 'b) (make-pos 'c)))
   (make-clauses (list (make-neg 'a) (make-neg 'c)))
   (make-clauses (list (make-neg 'd) (make-neg 'd)))))

(define duplicate-sample
  (list
   (make-clauses (list (make-pos 'a) (make-pos 'b)))
   (make-clauses (list (make-pos 'a) (make-pos 'b)))
   (make-clauses (list (make-neg 'b) (make-pos 'c)))
   (make-clauses (list (make-neg 'a) (make-neg 'c)))
   (make-clauses (list (make-neg 'd) (make-neg 'd)))))

;;CONSTANT-DEFINITIONS:
(define INITIAL-VALUE 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION-DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;; clauses using the rule of resolution as given in the problem statement.
;; EXAMPLES: (is-null-derivable? sample-1) -> true
;; STRATEGY: Divide into cases on loc and use the template of loc

(define (is-null-derivable? loc)
  (cond
    [(empty? loc) false]
    [(equal? (list empty) loc) true]
    [else
     (initially-possible?
      (map
       ;;List -> Boolean
       ;; GIVEN : List
       ;; RETURNS : boolean value
       (lambda (x)
         (list x))
       loc) loc)]))

;;TESTS:
(begin-for-test
  (check-equal?
   (is-null-derivable? sample-2)false
   "Should return false as  null is not derivable from input list of clause")
  (check-equal?
   (is-null-derivable? sample-3)true
   "Should return true as null is derivable from input list of clause")
  (check-equal?
   (is-null-derivable? sample)true
   "Should return true as null is derivable from input list of clause")
  (check-equal?
   (is-null-derivable? sample-4)false
   "Should return false as  null is not derivable from input list of clause")
  (check-equal?
   (is-null-derivable? empty)false
   "Should return false as  null is not derivable from empty list of clause")
  (check-equal?
   (is-null-derivable? (list empty))true
   "Should return true as null is derivable from list of clause with no clauses in list"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initially-possible? : List ListOfClause -> Boolean
;; GIVEN : List and List of clause
;; WHERE: l is a list of ListOfClause that stores possible combinitions
;;        of clauses.
;; RETURNS: true iff the empty clause is derivable from the given
;; clauses using the rule of resolution as given in the problem statement.
;; EXAMPLE : (initially-possible? (list (list (make-clauses
;;            (list (make-pos 'a) (make-pos 'b))))) sample-4) -> false
;; STRATEGY : Combine simple functions
(define (initially-possible? l loc)  
  (null-derivable-possible? (create-subset l loc) INITIAL-VALUE loc))

;;TESTS:
(begin-for-test
  (check-equal?
   (initially-possible?
    (list (list (make-clauses (list (make-pos 'a) (make-pos 'b))))) sample-4)
   false
   "Should return false as initial clause does not give empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; null-derivable-possible?: List Integer ListOfClause -> Boolean
;; GIVEN : List,integer and list of clause
;; WHERE: l is a list of ListOfClause that stores possible combinitions
;;    of clauses, and n indicate the time for combinition, larger than 1
;;    and smaller than the length of ListOfClause.
;; RETURNS: true iff the empty clause is derivable from the given
;; clauses using the rule of resolution as given in the problem statement.
;; EXAMPLE :(null-derivable-possible?  (list (list (make-clauses (list (make-pos 'a) (make-pos 'b)))))  2
;;  (list (make-clauses (list (make-pos 'a) (make-pos 'b)))  (make-clauses (list (make-neg 'a) (make-pos 'b)))
;;  (make-clauses (list(make-pos 'a) (make-neg 'b))))) -> false
;; HALTING MEASURE: the value of n
;; STRATEGY : Use HOF ormap on l
(define (null-derivable-possible? l n loc)
  (if (equal? (- n 1) (length loc)) false
      (or
       (ormap
        ;; Clause -> ListOfClause
        ;; GIVEN : Clause
        ;; RETURNS : ListOfClause
        (lambda (x) (clause-matches? (first x) (rest x))) l)
       (null-derivable-possible? (create-subset l loc) (+ n 1) loc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clause-matches? : Clause ListOfClause -> Boolean
;; GIVEN: clause and a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;; clauses using the rule of resolution as given in the problem statement.
;; EXAMPLES: (clause-matches? (make-clauses (list (make-pos 'a))) (list (make-clauses
;; (list (make-pos 'a) (make-neg 'b))))) -> false
;; HALTING MEASURE: the length of loc
;; STRATEGY: Divide into cases based on List of clause

(define (clause-matches? c loc)
  (cond
    [(empty? loc) false] 
    [(check-multi-resolve? c (first loc))
     (if (empty? (rest loc)) false
         (clause-matches? (first (rest loc)) (rest (rest loc))))] 
    [(equal? (clauses-sol (literal-matches c (first loc))) empty) true]
    [else
     (clause-matches? (literal-matches c (first loc)) (rest loc))]))

;;TESTS:
(begin-for-test
  (check-equal?
   (clause-matches?  (make-clauses(list (make-pos 'a) (make-neg 'b))) sample-2)
   false
   "Should return false as clause did not find match"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-multi-resolve? :  Clause  Clause -> Boolean
;; GIVEN: clause and a list of clauses
;; RETURNS: true if there are multiple resolutions between two clauses.
;; EXAMPLES:(check-multi-resolve? (make-clauses(list (make-pos 'a) (make-neg 'b))) (make-clauses
;;       (list (make-pos 'b) (make-neg 'a)))) ->   true
;; STRATEGY : Use the template of clauses

(define (check-multi-resolve? c1 c2)
  (< (length (clauses-sol (literal-matches c1 c2)))    
     (-  (length (set-union (clauses-sol c1) (clauses-sol c2))) 2))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; literal-matches: Clause Clause -> ListOfLiteral
;; GIVEN: two clauses
;; RETURNS: A list of literal after matching up
;; EXAMPLES:(literal-matches (make-clauses (list (make-pos 'a) (make-neg 'b))) 
;; (make-clauses (list (make-pos 'b) (make-neg 'a)))) -> (make-clauses '())
;; STRATEGY : Use HOF ormap on clause

(define (literal-matches c1 c2)
  (if (ormap
       ;; Clause -> Boolean
       ;; GIVEN : clause
       ;; RETURNS : Boolean value
       (lambda (x) (check-literal? x c2)) (clauses-sol c1))
      (make-clauses
       (set-union (clause-set-diff (clauses-sol c1) (clauses-sol c2))
                  (clause-set-diff (clauses-sol c2) (clauses-sol c1))))
      c1)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-literal? : literal Clause -> Boolean
;; GIVEN: A literal and a clause
;; RETURNS: true if the literal matches to the clause
;; EXAMPLES:(check-literal? (make-pos 'a)
;;    (make-clauses (list (make-pos 'a) (make-neg 'b)))) -> false
;; STRATEGY : Use HOF map on literal
(define (check-literal? l c)
  (ormap
   ;; Literal -> Boolean
   ;; GIVEN : literal
   ;; RETURNS : Boolean
   (lambda (x)
     (if (equal-literal? x l) true false)) (clauses-sol c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equal-literal? : literal literal -> Boolean
;; GIVEN : two literals
;; RETURNS:  true if literals matches to each other
;; EXAMPLES: (equal-literal? (make-pos 'a) (make-pos 'a)) -> false
;; STRATEGY : Use template of literal
(define (equal-literal? l1 l2)
  (cond
    [(and (pos? l1) (neg? l2)) (equal? (pos-variable l1) (neg-variable l2))]
    [(and (neg? l1) (pos? l2)) (equal? (pos-variable l2) (neg-variable l1))]
    [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clause : ListOfLiteral -> Clause
;; GIVEN :a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;; EXAMPLES: (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c) (make-pos 'd)))
;; -> (make-clauses (list (make-pos 'a) (make-pos 'b) (make-pos 'c) (make-pos 'd)))
;; STRATEGY : Use the template of clauses
(define (make-clause lol)
  (make-clauses (remove-duplicate lol)))

;;TESTS:
(begin-for-test
  (check-equal?
   (make-clause duplicate-sample)
   (make-clauses
    (list
     (make-clauses (list (make-pos 'a) (make-pos 'b)))
     (make-clauses (list (make-neg 'b) (make-pos 'c)))
     (make-clauses (list (make-neg 'a) (make-neg 'c)))
     (make-clauses (list (make-neg 'd) (make-neg 'd)))))
   "Should return input clause without any duplicate clauses"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-duplicate : ListOfLiteral -> SetOfLiteral
;; GIVEN :a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly list of literal without duplicates.
;; EXAMPLES:(remove-duplicate (list (make-pos 'a) (make-neg 'b)(make-neg 'b) (make-pos 'c) )) 
;;        -> (list (make-pos 'a) (make-neg 'b) (make-pos 'c))
;; STRATEGY : Use template of ListofLiteral and HOF ormap, foldr on listofliteral

(define (remove-duplicate lol) 
  (foldr
   ;; Clause Clause -> ListofClause
   ;; GIVEN : two clauses to compare
   ;; RETURNS : List of Clause
   (lambda (x y)
     (cons x 
           (filter
            ;; Clause -> Boolean
            ;; GIVEN : Clause 
            ;; RETURNS : Boolean
            (lambda (z)
              (not (equal? x z))) y))) empty lol))
;;TESTS:
(begin-for-test
  (check-equal? (remove-duplicate 
                 (list
                  (make-clauses (list (make-pos 'a) (make-pos 'b) ))
                  (make-clauses (list (make-pos 'a) (make-pos 'b) ))
                  (make-clauses (list (make-neg 'b) (make-pos 'c)))
                  (make-clauses (list (make-neg 'a) (make-neg 'c)))
                  (make-clauses (list (make-neg 'd) (make-neg 'd))))) sample-4
                                                                      "Should return list of clauses without any duplicate clauses"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-subset : List ListOfClause -> List
;; GIVEN : list and list of clause
;; WHERE: list is a list of ListOfClause that stores possible combinitions
;;        of clauses.
;; RETURNS : List of possible combinations of clauses
;; EXAMPLES :(create-subset(list (list (make-clauses
;;           (list (make-pos 'a) (make-pos 'b))))) sample-4)(list
;; (list (make-clauses (list (make-neg 'b) (make-pos 'c))) (make-clauses
;; (list (make-pos 'a) (make-pos 'b))))
;; (list (make-clauses (list (make-neg 'a) (make-neg 'c)))
;;       (make-clauses (list (make-pos 'a) (make-pos 'b))))
;; (list (make-clauses (list (make-neg 'd) (make-neg 'd)))
;;       (make-clauses (list (make-pos 'a) (make-pos 'b)))))
;; HALTING MEASURE: the length of loc
;; STRATEGY : Divide into cases based on List of clauses
(define (create-subset list loc)  
  (cond 
    [(empty? loc) empty] 
    [else
     (set-union
      (create-subset-1 list empty (first loc))
      (create-subset list (rest loc)))]))

;;TESTS :
(begin-for-test
  (check-equal?
   (create-subset
    (list (list (make-clause(list (make-pos 'a) (make-pos 'b)))))
    duplicate-sample)
   (list(list
         (make-clauses (list (make-neg 'b) (make-pos 'c)))
         (make-clauses (list (make-pos 'a) (make-pos 'b))))
        (list (make-clauses (list (make-neg 'a) (make-neg 'c)))
              (make-clauses (list (make-pos 'a) (make-pos 'b))))
        (list (make-clauses (list (make-neg 'd) (make-neg 'd)))
              (make-clauses (list (make-pos 'a) (make-pos 'b)))))
   "Should return all poosible combinations of clauses"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-subset-1: ListofClause ListofClause Clause -> ListOfClauses
;; GIVEN : Input list of clause and list of clause , clause
;; RETURNS : List of clause as the combintion of ListOfClause and the clause
;; EXAMPLES :(create-sub-mid-1(list (list (make-clauses
;;         (list (make-pos 'a) (make-pos 'b))))) (list (list (make-clauses
;;          (list (make-pos 'c) (make-pos 'b))))) (make-clauses
;;          (list (make-pos 'c) (make-pos 'd)))) 
;; (list (list (make-clauses (list (make-pos 'c) (make-pos 'd))) (make-clauses (list
;;       (make-pos 'a) (make-pos 'b)))) (list (make-clauses (list (make-pos 'c) (make-pos 'b)))))
;; HALTING MEASURE: the length of loc1
;; STRATEGY : Use template of list of clause
(define (create-subset-1 loc1 loc2 c)
  (if (empty? loc1) loc2
      (create-subset-1 (rest loc1) (create-subset-2 (first loc1) c loc2) c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-sub-mid-2: ListOfClause Clause -> ListOfClause 
;; GIVEN : Input list of clause , clause and list of clauses
;; RETURNS : List of clause as the combinition of ListOfClause and the clause
;; EXAMPLES :(create-sub-mid-1(list (list (make-clauses 
;;         (list (make-pos 'a) (make-pos 'b))))) (list (list (make-clauses
;;          (list (make-pos 'c) (make-pos 'b))))) (make-clauses
;;          (list (make-pos 'c) (make-pos 'd)))) 
;; (list (list (make-clauses (list (make-pos 'c) (make-pos 'd))) (make-clauses (list
;;       (make-pos 'a) (make-pos 'b)))) (list (make-clauses (list (make-pos 'c) (make-pos 'b)))))
;; STRATEGY : Combine simpler functions
(define (create-subset-2 loc c ori) 
  (if (or 
       (losoc-member? (clause-set-cons c loc) ori) 
       (clause-set-member? c loc))
      ori
      (cons (clause-set-cons c loc) ori))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; losoc-member? : ListClause List -> Boolean
;; GIVEN: ListClause, ListOfClause
;; WHERE: losoc is a list of ListOfClause that stores possible combinitions
;;        of clauses.
;; RETURNS : true if the ListOfClause is a member of the losoc
;; EXAMPLES: (losoc-member?(list (make-clauses (list (make-pos 'a) (make-pos 'b)))) (list (list (make-clauses
;;        (list (make-pos 'c) (make-pos 'b)))))) -> false
;; STRATEGY : Use HOF ormap on List of clause
(define (losoc-member? x losoc)
  (ormap
   ;;Set->Boolean
   ;;GIVEN:  set of clause
   ;;RETURNS: boolean value
   (lambda (y)
     (clause-set-equal? x y))
   losoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clause-set-equal? : SetOfClause SetOfClause -> boolean
;; GIVEN : Setof clauses
;; RETURNS : true if two setofclauses have same elements
;; EXAMPLES : (clause-set-equal? (list (make-clauses (list (make-pos 'a) (make-pos 'b)))) (list (make-clauses
;;          (list (make-pos 'c) (make-pos 'b))))) -> false
;; STRATEGY Use HOF andmap on set of clause
(define (clause-set-equal? soc1 soc2)
  (andmap
   ;;Element -> Boolean
   ;;GIVEN : Element
   ;;RETURNS : boolean value
   (lambda (t)
     (clause-set-member? t soc2)) 
   soc1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clause-set-member? : X SetOf<X> -> Boolean
;; GIVEN : Element of set and set of clauses
;; RETURNS : boolean value
;; EXAMPLES : (clause-set-member? (make-clauses (list (make-pos 'a) (make-pos 'b))) sample-4) -> true
;; STRATEGY: Use HOF ormap on clause
(define (clause-set-member? x set1)
  (ormap
   ;;Clause -> Boolean
   ;;GIVEN : clause
   ;;RETURNS : boolean value
   (lambda (z) (set-equal? (clauses-sol x) (clauses-sol z)))
   set1))

;;TESTS :
(begin-for-test
  (check-equal?
   (clause-set-member?
    (make-clauses (list (make-pos 'a) (make-pos 'c) (make-neg 'b))) sample-2)
   true "Should return true as it is member of input list of clause"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clause-set-cons : X SetOf<X> -> SetOf<X>
;; GIVEN : Set Element and set of clauses
;; RETURNS : Set of clauses
;; EXAMPLES : (clause-set-cons (make-clauses (list (make-pos 'a) (make-pos 'b))) sample-4)
;; -> (list(make-clauses (list (make-pos 'a) (make-pos 'b)))
;; (make-clauses (list (make-neg 'b) (make-pos 'c)))
;; (make-clauses (list (make-neg 'a) (make-neg 'c)))
;; (make-clauses (list (make-neg 'd) (make-neg 'd)))
;; STRATEGY : Combine simple functions
(define (clause-set-cons x set1)
  (if (clause-set-member? x set1) 
      set1
      (cons x set1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-union : SetOf<X> SetOf<X> -> SetOf<X>
;; GIVEN : two set of clauses
;; RETURNS : union of input Set of clauses
;; EXAMPLES : (clause-set-union sample sample-1)
;; (list(make-clauses (list (make-pos 'a)))
;; (make-clauses (list (make-neg 'a)))(make-clauses (list (make-pos 'a) (make-neg 'b) (make-pos 'c)))
;; (make-clauses (list (make-neg 'a) (make-pos 'c)))(make-clauses (list (make-pos 'b)))
;; (make-clauses (list (make-neg 'c))))
;; STRATEGY : Use HOF foldr on set

(define (clause-set-union set1 set2)
  (if (empty? set2) set1
      (foldr
       clause-set-cons
       set2
       set1)))

;;TESTS :
(begin-for-test
  (check-equal?
   (clause-set-union sample sample-1)
   (list(make-clauses (list (make-pos 'a)))
        (make-clauses (list (make-neg 'a)))(make-clauses (list (make-pos 'a) (make-neg 'b) (make-pos 'c)))
        (make-clauses (list (make-neg 'a) (make-pos 'c)))(make-clauses (list (make-pos 'b)))
        (make-clauses (list (make-neg 'c)))) "Should return union of two lists")
  (check-equal?
   (clause-set-union sample  empty)
   (list (make-clauses (list (make-pos 'a))) (make-clauses (list (make-neg 'a))))
   "Should return union of two lists"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clause-set-diff : Set Set -> Set of Clauses 
;; GIVEN : two sets
;; RETURNS : Set of Clauses without complimentary literals
;; EXAMPLES : (clause-set-diff sample sample-1)
;; -> (list (make-clauses (list (make-pos 'a))) (make-clauses (list (make-neg 'a))))
;; STRATEGY : Use HOF filter and ormap on set

(define (clause-set-diff set1 set2)
  (filter
   ;; Element -> Boolean
   ;; GIVEN : element of set1
   ;; RETURNS : true if two literals could be resolved
   (lambda (element)
     (not
      (ormap
       ;;Literal Literal -> Boolean
       ;;GIVEN : two Literals
       ;;RETURNS : true if two literal could be resolved
       (lambda (x)
         (equal-literal? x element))set2)))
   set1))   

;;TESTS :
(begin-for-test
  (check-equal?
   (clause-set-diff sample duplicate-sample)
   (list (make-clauses
          (list (make-pos 'a))) (make-clauses (list (make-neg 'a))))
   "Should return Set of Clauses with complimentary literals"))    




;;; Note: The original version of this benchmark defined a function
;;; that didn't satisfy its contract.  That function was not needed
;;; by the benchmark and has been removed.  I have also added a call
;;; to make-clause.

;;; make-stress-input-sat : NonNegInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: a satisfiable set of clauses of length n
;;; EXAMPLES:
;;;     (make-stress-input-sat 0) => empty
;;;     (make-stress-input-sat 3)
;;;  => (list (make-clause (list (make-pos 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-pos 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-pos 'p3))))

;(define (make-stress-input-sat n)
;  (local ((define (reverse-iota k)
;            (if (= k 0)
;                empty
;                (cons k (reverse-iota (- k 1)))))
;          (define (iota k)
;            (reverse (reverse-iota k))))
;    (let* ((nums (iota n))
;           (syms (map (lambda (k)
;                        (string->symbol (string-append "p"
;                                                       (number->string k))))
;                      nums)))
;      (map (lambda (k)
;             (make-clause   ; see note above
;              (map (lambda (i)
;                     ((if (= i k) make-pos make-neg)
;                      (list-ref syms (- i 1))))
;                   nums)))
;           nums))))
;
;;;;; stress-benchmark2 : NonNegInt -> Boolean
;;;;; GIVEN: a non-negative integer n
;;;;; RETURNS: false
;;;;; EFFECT: reports how many milliseconds it takes to determine
;;;;;     (make-stress-input-sat n) is satisfiable
;; 
;;  (time (is-null-derivable? (make-stress-input-sat n)))
;; 
;;;; stress-benchmark2 : NonNegInt -> Boolean
;;;; GIVEN: a non-negative integer n
;;;; RETURNS: false
;;;; EFFECT: reports how many milliseconds it takes to determine
;;;;     (make-stress-input-sat n) is satisfiable
;
;(define (stress-benchmark2 n)
;  (time (is-null-derivable? (make-stress-input-sat n))))