;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "07" "q1.rkt")


(provide
 program-to-strings
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

;;TESTING-VARIABLES:

(define DEF "def ")
(define sample-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))

(define sample-1
  (list
   (make-def 'list-function-names
             (list 'f1 'f2 'f3 'f4 'f5)
             (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))))
   (make-def 'list-variable-names
             (list 'f1 'f2 'f3 'f4 'f5)
             (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'y) (make-varexp 'z))))
   (make-def 'list-names (list 'x 'y 'z)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'x)
                               (make-varexp 'z)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))
(define sample-2
  (list
   (make-def 'list-function-names
             (list 'f1 'f2 'f3)
             (make-appexp 'f1 (list (make-varexp 'a)))) 
   (make-def 'list-variable-names 
             (list 'x1 'y1 'z1) 
             (make-varexp 'a))
   (make-def 'list-names (list 'f11 'f22 'f33)
             (make-appexp
              'f11
              (list (make-appexp
                     'f33 (list (make-varexp 'a)
                                (make-varexp 'b)))
                    (make-appexp
                     'f22 (list (make-varexp 'p)
                                (make-appexp 'f34 (list (make-varexp 'a)
                                                        (make-appexp 'f35 (list (make-varexp 'a)
                                                                                (make-varexp 'b))))))))))))



(define output-1  (list
                   "def list-function-names (f1,"
                   "                         f2,"
                   "                         f3,"
                   "                         f4,"
                   "                         f5) :"
                   "    f1(x, y)"
                   "def list-variable-names (f1,"
                   "                         f2,"
                   "                         f3,"
                   "                         f4,"
                   "                         f5) :"
                   "    f2(x, y, z)"
                   "def list-names (x,"
                   "                y,"
                   "                z) :"
                   "    f1(f2(x, z),"
                   "       z,"
                   "       f1(f2(z, y),"
                   "          z))"))
(define sample-3
  (list
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list
               (make-varexp 'z)
               (make-appexp
                'f1 (list (make-appexp
                           'f2 (list (make-varexp 'z)
                                     (make-varexp 'y)))
                          (make-varexp 'z))))))))

(define output-3
  (list "def f3 (x," "        z," "        t," "        u) :" "    f1" "     (z," "      f1" "       (f2" "         (z," "          y)," "        z))"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION-DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described below.
;; EXAMPLES:(program-to-strings sample-program 10) ->
;; HALTING MEASURE: the number of definitions in program
;; STRATEGY: Use the template of ListOfDefinition

(define (program-to-strings p w)
  (cond
    [(empty? p)empty]
    [else
     (append
      (def-to-string (first p) w)
      (program-to-strings (rest p) w))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (program-to-strings sample-3 6) output-3
   "Should return formatted header and body according to mentioned rules")
  (check-equal?
   (program-to-strings sample-1 20) output-1
   "Should return formatted header and body according to mentioned rules"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def-to-string : Definition PosInt -> ListOfString
;; GIVEN: Definition and width
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS: a list of string
;; EXAMPLES: (def-to-string (make-def 'list-function-names
;             (list 'f1 'f2 'f3 'f4 'f5)
;             (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y)))) 10)
;->(list
; "def list-function-names (f1,"
; "                         f2,"
; "                         f3,"
; "                         f4,"
; "                         f5) :"
; "    f1(x,"
; "       y)")
;; STRATEGY: Use HOF map on a ListOfString and the template of def

(define (def-to-string d w)
  (append 
   (check-header (def-name d) (def-args d) w)
   (map
    (lambda(x) (string-append (create-space 4 "") x))
    (check-body (def-body d) (- w 4)))))

;;TESTS:
(begin-for-test
  (check-equal? (def-to-string
                  (make-def 'list-function-names
                            (list 'f1 'f2 'f3 'f4 'f5) (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y)))) 10)
                (list
                 "def list-function-names (f1,"
                 "                         f2,"
                 "                         f3,"
                 "                         f4,"
                 "                         f5) :"
                 "    f1(x,"
                 "       y)") "Should return formatted definition"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-header : Variable ListOfVariable PosInt-> ListOfString
;; GIVEN: Definition name, arguements and width
;; RETURNS: a list of string
;; EXAMPLES:(check-header 'fn (list 'arg1 'arg2) 10)
;;  ->(list "def fn (arg1," "        arg2) :")
;; STRATEGY: Divide into cases based on the length of args

(define (check-header name args w)
  (cond
    [(equal? (length args) 1)
     (list (create-header name args DEF))]
    [(> (string-length (create-header name args DEF)) w)
     (format-header name args (list DEF))]
    [else
     (list (create-header name args DEF))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (check-header 'f1 (list 'arg1) 1)(list "def f1 (arg1) :")
   "Should return header formatted on single line")
  (check-equal?
   (check-header 'f2 (list 'arg1 'arg2 'arg3 'arg4) 35)
   (list "def f2 (arg1,arg2,arg3,arg4) :")
   "Should return header formatted on single line as length of header is less than given width"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-header : Variable ListOfVariable String -> String
;; GIVEN: Definition name and arguements and string
;; RETURNS: a string with all infos of the expression header.
;; EXAMPLES:(create-header 'fn (list 'arg1 'arg2) DEF) -> "def fn (arg1,arg2) :"
;; HALTING MEASURE: the number of elements in args
;; STRATEGY: Divide into cases based on args and str

(define (create-header name args str)
  (cond
    [(empty?  args)  (string-append str ") :")]
    [(equal? str DEF)
     (create-header
      name (rest args)
      (string-append str (symbol->string name) " (" (symbol->string (first args))))]
    [else
     (create-header
      name (rest args)
      (string-append str "," (symbol->string (first args))))]))

;;TESTS:
(begin-for-test
  (check-equal? (create-header 'f1 (list 'x 'y) "z")
                "z,x,y) :" "Should return created header"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; format-header : Variable ListOfVariable ListOfString  -> ListOfString
;; GIVEN: Definition  name and arguements  and list of string
;; RETURNS: a list of string
;; EXAMPLES:(format-header 'fn (list 'arg1 'arg2) (list "1" "2"))
;; ->(list "2" "1" "        arg1," "        arg2) :")
;; HALTING MEASURE: the number of elements in args
;; STRATEGY:Divide into cases and use the template of ListOfVariable

(define (format-header name args los)
  (cond
    [(empty? (rest args)) (format-header-last name args los)]
    [(equal? los (list DEF)) (format-header-initial name args los)]
    [else (format-header-middle name args los)]))

;;TESTS:
(begin-for-test
  (check-equal? (format-header 'fn (list 'arg1 'arg2) (list "1" "2"))
                (list "2" "1" "        arg1," "        arg2) :") "Should return formatted header"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-space: PosInt String-> String
;; GIVEN: a number and a string
;; WHERE: n is a positive integer and creates indentation of n value
;; RETURNS: a space string with length of PosInt
;; EXAMPLES:(create-space 5 "class") -> "     class"
;; HALTING MEASURE: the value of n
;; STRATEGY: Combine simpler functions
(define (create-space n str)
  (if (equal? n 0)
      str
      (create-space (- n 1) (string-append " " str))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-header-last: Variable ListOfVariable ListOfString -> ListOfString
;; GIVEN : Definition  name and arguements  and list of string
;; RETURNS: a list of string which is listed on the last part
;; EXAMPLES:(format-header-last 'fun (list 'a1 'a2) (list "a" "b" "c")) ->
;;          (list "c" "b" "a" "         a1) :")
;; STRATEGY : Use the template of ListOfVariable 
(define (format-header-last name args los)
  (reverse
   (cons
    (string-append
     (create-space
      (string-length (string-append DEF (symbol->string name) " (")) "")
     (symbol->string (first args)) ") :") los)))
;;TESTS:
(begin-for-test
  (check-equal? (format-header-last 'fn (list 'a1 'a2) (list "a" "b" "c"))
                (list "c" "b" "a" "        a1) :") "Should return formatted list of string of header"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-header-initial: Variable ListOfVariable ListOfString -> ListOfString
;; GIVEN : Definition name and arguements  and list of string
;; RETURNS: a list of string which is listed on the first part
;; EXAMPLES:(format-header-initial 'fun (list 'a1 'a2) (list "a" "b" "c")) ->
;;          (list "def fun (a1," "         a2) :")
;; STRATEGY : Use the template of ListOfVariable
(define (format-header-initial name args los)
  (format-header
   name (rest args)
   (list
    (string-append DEF (symbol->string name) " (" (symbol->string (first args)) "," ))))

;;TESTS:
(begin-for-test
  (check-equal? (format-header-initial 'f1 (list 'x 'y) (list "a" "b" "c"))
                (list "def f1 (x," "        y) :")  "Should return formatted function name of header"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-header-middle: Variable ListOfVariable ListOfString -> ListOfString
;; GIVEN : Definition  name and arguements  and list of string
;; RETURNS: a list of string which is listed on the middle part
;; EXAMPLES:(format-header-middle 'fun (list 'a1 'a2) (list "a" "b" "c")) ->
;;          (list "c" "b" "a" "         a1," "         a2) :")
;; STRATEGY : Use the template of ListOfVariable
(define (format-header-middle name args los)
  (format-header
   name (rest args)
   (cons
    (string-append
     (create-space
      (string-length (string-append DEF (symbol->string name) " (")) "")
     (symbol->string (first args)) "," ) los)))

;;TESTS:
(begin-for-test
  (check-equal? (format-header-middle 'f1 (list 'x 'y) (list "a" "b" "c"))
                (list "c" "b" "a" "        x," "        y) :") "Should return formatted arguments part of header"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-body : Exp PosInt-> ListOfString
;; GIVEN: An expression and a width
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS: a list of string with the required format
;; EXAMPLES:(check-body (make-appexp 'f1 (list (make-varexp 'x))) 10) -> (list "    f1(x)")
;; STRATEGY:Combine simpler functions

(define (check-body exp w)
  (if
   (> (string-length (create-body-one-line exp "")) w)
   (format-body exp w)
   (list (create-body-one-line exp ""))))

;; TESTS:
(begin-for-test
  (check-equal?
   (check-body (make-appexp 'f1 (list (make-varexp 'x)  (make-varexp 'y)
                                      (make-varexp 'z))) 35)
   (list "f1(x, y, z)") "Should return body with spaced 4 and on single line")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-body-one-line: Exp String -> String
;; GIVEN: an Expression and a string
;; RETURNS: an string containing the content of the expression on one line
;; EXAMPLES: (create-body-one-line (make-appexp 'f1 (list (make-varexp 'x))) "varexp") -> "varexpf1(x)"
;; STRATEGY: Use the template of Expression, varexp and appexp
(define (create-body-one-line exp str)
  (cond
    [(varexp? exp)
     (string-append str (symbol->string (varexp-name exp)))]
    [(appexp? exp)
     (string-append str (symbol->string (appexp-fn exp)) "("
                    (create-loe-one-line (appexp-args exp) ""))]))

;; TESTS:
(begin-for-test
  (check-equal? (create-body-one-line (make-varexp 'y) "varexp") "varexpy"
                "Should return variable expression with appended variable"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-loe-one-line: ListOfExpression String -> ListOfString
;; GIVEN: a list of expression and a string
;; RETURSNS: a string containing the content of the listofExpression.
;; EXAMPLES:(create-loe-one-line (list (make-appexp 'f1 (list (make-varexp 'x)))
;; (make-appexp 'f2 (list (make-varexp 'x)))) "varexp") -> "varexpf1(x), f2(x))"
;; HALTING MEASURE: the numbers of elements in loe
;; STRATEGY: Use the template of ListOfExpression and divide into cases based on loe
(define (create-loe-one-line loe str)
  (cond
    [(and (empty? (rest loe)) (varexp? (first loe)))
     (string-append str (symbol->string (varexp-name (first loe))) ")")]
    [(and (empty? (rest loe)) (appexp? (first loe)))
     (string-append (create-body-one-line (first loe) str) ")")]
    [(varexp? (first loe))
     (create-loe-one-line
      (rest loe)
      (string-append str (symbol->string (varexp-name (first loe))) ", "))]
    [(appexp? (first loe))
     (create-loe-one-line
      (rest loe)
      (string-append (create-body-one-line (first loe) str) ", "))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-body : Exp PosInt -> ListOfString
;; GIVEN : an Expression and a width
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string with the required format
;; EXAMPLE :(display-strings! (format-body (make-appexp 'f1 (list (make-varexp 'x)
;;  (make-varexp 'y) (make-varexp 'z))) 10))
;; ->f1(x,
;;      y,
;; ,    z)
;; STRATEGY : Use the template of appexp and ListOfVariables
(define (format-body exp w)
  (if
   (<= (string-length
        (create-loe-first-line
         (first (appexp-args exp))
         (string-append (symbol->string (appexp-fn exp)) "("))) w)
   (body-one-line exp w)
   (body-not-on-line exp w)))

;; TESTS:
(begin-for-test
  (check-equal?
   (format-body (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y)
                                       (make-varexp 'z))) 80)
   (list "f1(x," "   y," "   z)") "Should return formatted body on single line"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; body-one-line : Exp PosInt -> ListOfString
;; GIVEN : an Expression and a width
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string with the required format
;; EXAMPLES :(body-one-line (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y) (make-varexp 'z))) 10))
;;->f1(x," "   y," "  z)")
;; STRATEGY : Use HOF map on a ListOfString and template of appexp, ListOfExpression
(define (body-one-line exp w)
  (append
   (list
    (create-loe-first-line
     (first (appexp-args exp))
     (string-append (symbol->string (appexp-fn exp)) "(")))
   (map
    (lambda (x)
      (string-append
       (create-space
        (+ (string-length
            (symbol->string (appexp-fn exp))) 1) "") x))
    (format-loe
     (rest (appexp-args exp)) empty
     (- w (+ (string-length (symbol->string (appexp-fn exp))) 1)) false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; body-not-on-line : Exp PosInt -> ListOfString
;; GIVEN : an Expression and a width
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string with the required format
;; EXAMPLE : (body-not-on-line (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y) (make-varexp 'z))) 10))
;; ->f1(x,
;;      y,
;;      z)
;; STRATEGY : Use HOF map on a ListOfString
(define (body-not-on-line exp w)   
  (append
   (list (symbol->string (appexp-fn exp)))
   (map (lambda (y) (string-append (create-space 1 "") y))
        (format-loe (appexp-args exp) empty (- w 1) true))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-loe : ListOfExpression ListOfString PosInt Boolean-> ListOfString
;; GIVEN : a list of expression, a list of String, width  and a Boolean
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string of ListOfExpression with symbols converted to strings
;; EXAMPLES :(format-loe (list (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))))
;;             -> (list "a" "b") 10 true) (list "(a" " b" " f1(x, y))")
;; HALTING MEASURE: the number of elements in loe
;; STRATEGY : Combine simpler functions
(define (format-loe loe los w b)
  (if (empty? loe) empty
      (if b
          (format-loe-with-bracket loe los w b)
          (format-loe-without-bracket loe los w b))))
;TESTS:
(begin-for-test
  (check-equal? (format-loe (list (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))))
                            (list "a" "b") 10 true) (list "(a" " b" " f1(x, y))")
                                                    "Should return listofexpression with bracket")
  (check-equal? (format-loe empty
                            (list "a" "b") 10 true) empty
                                                    "Should return empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-loe-with-bracket : ListOfExpression ListOfString PosInt Boolean-> ListOfString
;; GIVEN : a list of expression, a list of String, width  and a Boolean
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string of ListOfExpression with symbols converted to strings
;; EXAMPLES : (format-loe (list (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))))
;;                     -> (list "a" "b") 10 true) (list "(a" " b" " f1(x, y))")
;; HALTING MEASURE: the number of elements in loe
;; STRATEGY : Divide into cases based on loe and use the template of ListOfExpression
(define (format-loe-with-bracket loe los w b)
  (cond
    [(empty? (rest loe))
     (format-loe-with-bracket-last (first loe) los w b)]
    [(varexp? (first loe))
     (move-backward-rest
      (format-loe
       (rest loe)
       (append los (list (string-append "(" (symbol->string (varexp-name (first loe))) ",")))
       (- w 2) false))]
    [(appexp? (first loe))
     (move-backward-rest
      (format-loe
       (rest loe)
       (add-punct
        (add-punct
         (append los (check-body (first loe) (- w 1))) "first" "bracket") "last" "comma")
       w false))]))
;TESTS:
(begin-for-test
  (check-equal? (format-loe (list (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))))
                            (list "a" "b") 10 true) (list "(a" " b" " f1(x, y))")
                                                    "Should return formatted list of expression with bracket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-loe-with-bracket-last : Expression ListOfString PosInt Boolean-> ListOfString
;; GIVEN : an expression, a list of String, width  and a Boolean
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string of ListOfExpression with symbols converted to strings
;; EXAMPLES : (format-loe-with-bracket-last (make-varexp 'x) (list "a" "b") 10 true)
;;           -> (list "a" "b" "(x)")
;; STRATEGY : Use the template of expression
(define (format-loe-with-bracket-last e los w b)
  (if (varexp? e)
      (append los (list (string-append "(" (symbol->string (varexp-name e)) ")")))
      (move-backward-rest
       (add-punct
        (add-punct (append los (check-body e (- w 1))) "first" "bracket") "last" "bracket"))))

;;TESTS:
(begin-for-test
  (check-equal? (format-loe-with-bracket-last (make-varexp 'x) (list "a" "b") 10 true)
                (list "a" "b" "(x)") "Should return list of string with bracket at last"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-loe-without-bracket : ListOfExpression ListOfString PosInt Boolean-> ListOfString
;; GIVEN : a list of expression, a list of String, width  and a Boolean
;; WHERE: width is a positive number and is the length of the definition
;; RETURNS : a list of string of ListOfExpression with symbols converted to strings
;; EXAMPLES : (format-loe-without-bracket
;                 (list
;                  (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y)))
;                  (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'y))))(list "a" "b") 5 true)
;->(list "a" "b" "f1(x," "   y)," "f2(x," "   y))")
;; HALTING MEASURE: the number of elements in loe
;; STRATEGY : Divide into cases based on the loe and use the template of ListOfExpression
(define (format-loe-without-bracket loe los w b)
  (cond
    [(empty? (rest loe))
     (format-loe-without-bracket-last (first loe) los w b)]
    [(varexp? (first loe))
     (format-loe
      (rest loe)
      (append
       los
       (list (string-append (symbol->string (varexp-name (first loe))) ","))) (- w 1) false)]
    [(appexp? (first loe))
     (format-loe
      (rest loe)
      (add-punct
       (append los (check-body (first loe) w))  "last" "comma") w false)]))

;; TESTS:
(begin-for-test
  (check-equal? (format-loe-without-bracket
                 (list
                  (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y)))
                  (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'y))))(list "a" "b") 5 true)
                (list "a" "b" "f1(x," "   y)," "f2(x," "   y))") "Should return punctuation after each exp in loe"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-loe-without-bracket-last : Expression ListOfString PosInt Boolean-> ListOfString
;; GIVEN : an expression, a list of String, width  and a Boolean
;; RETURNS : a list of string of ListOfExpression with symbols converted to strings
;; EXAMPLES : (format-loe-without-bracket-last (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))) (list "a" "b") 5 true)
;; ->(list "a" "b" "f1(x," "   y))")
;; STRATEGY : Use the template of appexp and varexp
(define (format-loe-without-bracket-last e los w b)
  (if (varexp? e)
      (append los (list (string-append (symbol->string (varexp-name e)) ")")))
      (add-punct (append los (check-body e w)) "last" "bracket")))

;; TESTS:
(begin-for-test
  (check-equal? (format-loe-without-bracket-last
                 (make-appexp 'f1 (list
                                   (make-varexp 'x)
                                   (make-varexp 'y)))
                 (list "a" "b") 5 true)
                (list "a" "b" "f1(x," "   y))") "Should return formatted list of expressions with bracket after last expression"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-backward: ListOfString -> ListOfString
;; GIVEN: a list of strings
;; RETURNS: a list of strings with a space added to all string in the list
;; EXAMPLES: (move-backward (list "x" "y" "z")) -> (list " x" " y" " z")
;; STRATEGY: Use HOF map on ListOfString

(define (move-backward los)
  (map (lambda (x) (string-append (create-space 1 "") x)) los))

;; TESTS:
(begin-for-test
  (check-equal? (move-backward (list "arg1" "arg2" "arg3"))(list " arg1" " arg2" " arg3")
                "Should return list of string with additional space"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-backward-rest: ListOfString -> ListOfString
;; GIVEN: a list of strings
;; RETURNS: a list of strings with a space added to all string except the first one
;; EXAMPLES:(move-backward-rest (list "x" "y" "z")) -> (list "x" " y" " z")
;; STRATEGY: Use HOF map on ListOfString
(define (move-backward-rest los)
  (cons
   (first los)
   (map (lambda (x) (string-append (create-space 1 "") x)) (rest los))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; add-punct: ListOfString String String-> ListOfString
;; GIVEN: a list of strings and two strings
;; RETURNS: a list string with a punctuation added according to the first string
;; EXAMPLES:(add-punct (list "x" "y" "z") "first" "bracket") -> (list "(x" "y" "z")
;; STRATEGY: Use the template of ListOfString
(define (add-punct los str sym)
  (cond
    [(and (equal? str "first") (equal? sym "comma"))
     (cons (string-append "," (first los)) (remove (list-ref los 0) los))]
    [(and (equal? str "last") (equal? sym "comma"))
     (append (remove (list-ref los (- (length los) 1)) los)
             (list (string-append (list-ref los (- (length los) 1)) ",")))]
    [(and (equal? str "first") (equal? sym "bracket"))
     (cons (string-append "(" (first los)) (remove (list-ref los 0) los))]
    [(and (equal? str "last") (equal? sym "bracket"))
     (append (remove (list-ref los (- (length los) 1)) los)
             (list (string-append (list-ref los (- (length los) 1)) ")")))])) 
;; TESTS:
(begin-for-test
  (check-equal? (add-punct (list "a" "b") "first" "comma")
                (list ",a" "b") "Should return comma before the first element in list of strings"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-loe-first-line: Expression String -> ListOfString
;; GIVEN: an expression and a string
;; RETURSNS: a string containing the content of the elements of the Expression.
;; EXAMPLES:(create-loe-first-line (make-appexp 'f1 (list (make-varexp 'y))) "x") -> "xf1(y),"
;; STRATEGY: Use the template of expression
(define (create-loe-first-line exp str)
  (cond
    [(varexp? exp) (string-append str (symbol->string (varexp-name exp)) ",")]
    [(appexp? exp) (string-append (create-body-one-line exp str) ",")]))
;; TESTS:
(begin-for-test
  (check-equal? (create-loe-first-line (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'y))) "z") 
                "zf1(x, y)," "Should return list of expression containing all expressions"))
