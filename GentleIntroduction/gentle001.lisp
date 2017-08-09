;; A Gentle Introduction to Common Lisp
;; ------------------------------------

;; Chapter 01 :: Functions and Data 
;; ----------

;; Functions that end with a "p" are generally test that return either
;; T, or NIL. The "p" stands for "predicate".

;; For example:
;; (numberp 2)
;; T
;; (numberp "a")
;; NIL
;; (symbolp 42)
;; NIL
;; (zerop 35)
;; NIL
;; (zerop 0)
;; T
;; (oddp 28)
;; NIL
;; (oddp 27)
;; T
;; (evenp 27)
;; NIL

;; By now you've caught on to the convention of tacking a "p" onto a function
;; name to show that it is a predicate.

;; Symbols
;; -------
;; Symbols are another data type in Lisp. Most symbols are typically named after
;; English words.

;; Putting Functions Together
;; --------------------------
;; The built-in functions are called "primitive functions" or "primitives". We
;; make new functions by putting primitives together in various ways.

;; Defining the function add1: Our function will take a single number as input
;; and add one to it.
(defun add1(n)
  (+ 1 n))

;; Defining the function add2: We could define add2 out of two add1:
(defun add2(n)
  (add1 (add1 n)))

;; We can also make our own predicates. Predicates are functions that
;; return a result of T, or NIL. The twop function returns T if its input is 2.
(defun twop(n)
  (equal 2 n))

;; Exercises
;; ---------
;; 1. Define a "sub2" function that subtracts two from its input.
(defun sub2(n)
  (- n 2))

;; 2. Show how to write the function "twop" in terms of "zerop" and "sub2"


;; 3. The "half" function returns a number that is one-half of its input.
;;    Define "half" in two different ways.
(defun half1(n)
  (/ n 2))

(defun half2(n)
  (* n 0.5))

;; 4. Write a "multi-digit-p" predicate that returns T if its input is greater
;;    than 9.
(defun multi-digit-p(n)
  (> n 9))

;; 5. What does this function do to a number? (f n) => -n
(defun negatio(n)
  (- 0 n))

;; Summary
;; -------
;; * In this chapter we covered two types of data: numbers and symbols.
;; * Predicates are a special class of functions that use T and NIL to answer
;;   questions about their inputs.
;; * A function must have a definition before we can use it.
;; * Functions covered in this chapter:
;;                   * Arithmetic: + - * / abs sqrt
;;                   * Predicates: numberp, symbolp, zerop, oddp, eveno, <, >
;;                                 equal, not



