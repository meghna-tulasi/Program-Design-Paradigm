;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(check-location "08" "q1.rkt")


(provide
 any-loops?
 make-def
 make-varexp
 make-appexp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA-DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Program is a ListOfDefinition

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Variable is a Symbol

;; We could have represented variables using strings instead of
;; symbols, but using symbols makes it a little easier to build
;; examples. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; OBSERVER TEMPLATES:

;; We group these because def-fn and exp-fn are mutually recursive.

;; pgm-fn : Program -> ??
#;
(define (pgm-fn p)
  (lodef-fn p))

;; def-fn : Definition -> ??
#;
(define (def-fn d)
  (... (def-name d) (def-args d) (def-body d)))

;; exp-fn : Exp -> ??
#;
(define (exp-fn e)
  (cond
    [(varexp? e) (... (varexp-name e))]
    [(appexp? e) (... (appexp-fn e) (loexp-fn (appexp-args e)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ListOfSymbol is one of the following:
;;--empty
;;--(cons Symbol LOS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ListOfVariable is one of the following:
;;--empty
;;--(cons Variable LOV)

;;TEMPLATE:
;;lov-fn :LOV -?
;(define (lov-fn lov)
;  (cond
;    [(empty? lov)...]
;    [else (...
;           (varexp (first lov))
;           (lov-fn (rest lov)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ListOfExpression is one of the following:
;;--empty
;;--(cons Exp LOE)

;;TEMPLATE:
;;loexp-fn : LOE-?
;(define (loexp-fn loe)
;  (cond
;    [empty? loexp)...]
;    [else (...
;           (exp-fn (first loexp))
;           (loexp-fn (rest loexp))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ListOfDefinition is one of the following:
;;--empty
;;--(cons Def LOD)

;;TEMPLATE:
;;lodef-fn : LOD-?
;(define (lodef-fn lod)
;  (cond
;    [empty? lod)...]
;    [else (...
;           (def-fn (first lod))
;           (lodef-fn (rest lod))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTING-VARIABLES:
(define some-loops
  (list
   (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
   (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'u)
             (make-appexp 'f1 (list (make-appexp 'f4
                                                 (list (make-varexp 'u)
                                                       (make-varexp 'w)))
                                    (make-varexp 'z))))
   (make-def 'f4 (list 'x 'y)
             (make-appexp 'f5
                          (list (make-varexp 'y)
                                (make-varexp 'u))))
   (make-def 'f5 (list 'u)
             (make-appexp 'f2
                          (list (make-appexp 'f3 empty))))
   (make-def 'no-loop (list 'x) (make-varexp 'x))))

(define sample
  (list
   (make-def 'fun (list 'x 'y)
             (make-appexp 'addfun (list (make-varexp 'x) (make-varexp 'y))))
   (make-def 'addfun (list 'x 'y) (make-varexp 'x))))

(define list-of-args
  (list (make-appexp 'f7
                     (list (make-varexp 'a)
                           (make-appexp 'f8
                                        (list (make-varexp 'b)
                                              (make-varexp 'c)
                                              (make-appexp 'f6 (list ))))
                           (make-appexp 'f8
                                        (list (make-varexp 'b)))))
        (make-appexp 'f8
                     (list (make-varexp 'd)
                           (make-appexp 'f9 (list (make-varexp 'h)))))
        
        (make-appexp 'f9 (list (make-varexp 'd) (make-varexp 'h)))
        (make-appexp 'f9
                     (list (make-appexp 'f7 (list (make-varexp 'a)))
                           (make-varexp 'h)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p (that is, a GS program that obeys the
;;         restrictions listed above).
;; RETURNS: true iff there is some function f in p that calls itself
;;          either directly or indirectly, as in the example above.
;; EXAMPLES:(any-loops? some-loops) -> true
;; STRATEGY: Use HOF ormap on Program

(define (any-loops? p)
  (cond
    [(empty? p) false]
    [else
     (ormap
      ;; Definition -> Boolean
      ;; GIVEN: Function Definition
      ;; RETURNS: Boolean
      (lambda (x) (check-loop? x (list (def-name x)) p))
      p)]))



;;TESTS:
(begin-for-test
  (check-equal? (any-loops? some-loops) true
                "Should return true as few functions in p call itself
  directly or indirectly")
  (check-equal? (any-loops? sample) false
                "Should return false as no functions in p call itself
  directly or indirectly")
  (check-equal? (any-loops? '()) false
                "Should return fasle as there are no function definitons in
  program")
  (check-equal?
   (any-loops?
    (list (make-def 'fun (list 'x 'y)
                    (make-appexp 'fun (list (make-varexp 'x) (make-varexp 'y))))))
   true
   "Should return true as there is a infinite loop in program"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-loop?: Definition ListOfSymbol Program -> Boolean
;; GIVEN: Definition, list of symbols and program
;; WHERE: ListofSymbol is a list that stores the symbols which have appeared
;; RETURNS:true if there is infinite loop on the definition
;; EXAMPLES:(check-loop? (make-def 'f4 (list 'x 'y) (make-appexp 'f5 (list (make-varexp 'y)
;;          (make-varexp 'u)))) (list 'a 'b) some-loops) -> true
;; STRATEGY: Use template of Definition

(define (check-loop? d lst p)
  (check-exp? (def-body d) lst p))

;;TESTS:

(begin-for-test
  (check-equal?
   (check-loop?
    (make-def 'f4 (list 'x 'y)
              (make-appexp 'f5 (list (make-varexp 'y)
                                     (make-varexp 'u))))(list 'a 'b) some-loops)
   true "Should return true as function definition has loop")
  (check-equal?
   (check-loop?
    (make-def 'f1 (list 'x)
              (make-appexp 'no-loop (list (make-varexp 'x)))) (list 'a 'b) some-loops)
   false "Should return false as function definition has no loop"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-exp?: Expression ListOfSymbol Program -> Boolean
;; GIVEN: expression list of symbols and sgs program
;; WHERE: ListofSymbol is a list that stores the symbols which have appeared
;; RETURNS: true if there is infinite loop on the expression
;; EXAMPLES: (check-exp? (make-appexp 'f1 (list (make-varexp 'y))) (list 'a 'b) some-loops) -> false
;; HALTING MEASURE: Number of expressions for each definitions in the program
;; STRATEGY: Divide into cases on e
(define (check-exp? e lst p)
  (cond
    [(varexp? e) false]
    [(member? (appexp-fn e) lst) true]
    [else
     (or
      (check-loe? (remove-duplicates (appexp-args e))  lst p)
      (check-loop? (find-def (appexp-fn e) p) (set-cons (appexp-fn e) lst) p))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (check-exp? (make-appexp 'f1 (list (make-varexp 'y)))
               (list 'a 'b) some-loops)
   false "Should return false as expression contains varexp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-loe?: ListOfExpression ListOfSymbol Program -> Boolean
;; GIVEN: list of expression , list of symbols and program
;; WHERE: ListofSymbol is a list that stores the symbols which have appeared
;; RETURNS: true if there is infinite loop on the loe
;; EXAMPLES:(check-loe? (list (make-appexp 'f1 (list (make-varexp 'x))) (make-appexp 'f2
;;          (list (make-varexp 'y)))) (list 'a 'b) some-loops) -> false
;; HALTING MEASURE: number of expressions in the program
;; STRATEGY: Use HOF ormap on ListOfExpression
(define (check-loe? loe lst p)
  (if (empty? loe) false
      (or
       (ormap
        ;; Expression -> Boolean
        ;; GIVEN : Expression
        ;; RETURNS : Boolean
        (lambda (n) (check-exp? n lst p)) loe)
       (check-loe? (rest loe) lst p))))

;; TESTS:
(begin-for-test
  (check-equal?
   (check-loe?
    (list (make-appexp 'f2 (list (make-varexp 'x)))
          (make-appexp 'f4 (list (make-varexp 'y))))
    (list 'a 'b) some-loops) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-def: Symbol Program-> Definition
;; GIVEN: Symbol and program
;; RETURNS: Definition that matches the given symbol in the program  
;; EXAMPLES:(find-def 'f1 some-loops) -> (make-def 'f1 (list 'x)
;;          (make-appexp 'no-loop (list (make-varexp 'x))))
;; STRATEGY: Use HOF filter on Program
(define (find-def s p)
  (first
   (filter
    ;;Definition -> Boolean
    ;;GIVEN: Definition name
    ;;RETURNS:boolean value
    (lambda (x) (equal? (def-name x) s)) p)))

;; TESTS:
(begin-for-test
  (check-equal?
   (find-def 'f1 some-loops)
   (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
   "Should return definition that matches the given symbol in the program"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-duplicate : ListOfExpression -> ListOfExpression
;; GIVEN :a list of expression, possibly with duplications.
;; RETURNS: list of expression without duplicates.
;; EXAMPLES: (remove-duplicates (list (make-appexp 'f1
;;           (list (make-varexp 'x))) (make-appexp 'f2 (list (make-varexp 'y))))) ->  
;;           (list (make-appexp 'f1 (list (make-varexp 'x)))
;;           (make-appexp 'f2 (list (make-varexp 'y))))
;; STRATEGY : Use HOF foldr and filter on expression

(define (remove-duplicates loe)
  (foldr
   ;; Expression Expression -> ListofExpression
   ;; GIVEN : two Expression to compare
   ;; RETURNS : List of expression
   (lambda (x y)
     (cons x
           (filter
            ;; Expression -> Boolean
            ;; GIVEN : Expression 
            ;; RETURNS : Boolean
            (lambda (z)
              (not (equal? x z))) y))) empty loe))

;; TESTS:
(begin-for-test
  (check-equal?
   (remove-duplicates
    (list (make-appexp 'f1 (list (make-varexp 'x)))
          (make-appexp 'f2 (list (make-varexp 'y)))))   
   (list (make-appexp 'f1 (list (make-varexp 'x)))
         (make-appexp 'f2 (list (make-varexp 'y))))
   "Should return list of expression without duplicates"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;; make-stress-input-without-loops : PosInt -> Program
;;;; GIVEN: an integer n
;;;; RETURNS: an SGS program with no loops that defines n functions
;;;; EXAMPLES:
;;;;     (make-stress-input-without-loops 1)
;;;;  => (list
;;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x)))
;;;;
;;;;     (make-stress-input-without-loops 3)
;;;;  => (list
;;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x))
;;;;      (make-def 'f2 (list 'x 'y)
;;;;        (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x))))
;;;;      (make-def 'f3 (list 'x 'y)
;;;;        (make-appexp
;;;;         'f2
;;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x)))
;;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x))))))
;
;(define (make-stress-input-without-loops n)
;  (local (
;          ;;; Returns a list of 1 through k.
;          (define (iota k)
;            (reverse (reverse-iota k)))
;          (define (reverse-iota k)
;            (if (= k 0)
;                empty
;                (cons k (reverse-iota (- k 1)))))
;
;          ;;; Given the function names in reverse order,
;          ;;; returns their bodies in reverse order.
;          (define (make-bodies names)
;            (if (empty? (rest names))
;                (list (make-varexp 'x))
;                (let* ((bodies (make-bodies (rest names)))
;                       (body (first bodies))
;                       (name (first (rest names))))
;                  (cons (make-appexp name (list body body))
;                        bodies)))))
;    (let* ((nums (iota n))
;           (syms (map (lambda (k)
;                        (string->symbol (string-append "f"
;                                                       (number->string k))))
;                      nums))
;           (bodies (reverse (make-bodies (reverse syms)))))
;      (map (lambda (sym body)
;             (make-def sym (list 'x 'y) body))
;           syms
;           bodies))))
;
;;;; stress-benchmark1 : PosInt -> Boolean 
;;;; GIVEN: a positive integer n
;;;; RETURNS: false
;;;; EFFECT: reports how many milliseconds it takes to determine
;;;;     (make-stress-input-without-loops n) has no loops
;
;(define (stress-benchmark1 n)
;  (time (any-loops? (make-stress-input-without-loops n))))