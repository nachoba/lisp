;; Chapter 03 :: EVAL Notation
;; ----------

;; The EVAL function is the heart of Lisp. EVAL's job is to evaluate Lisp expressions
;; to compute their result.
;; (eval (+ 2 3))   => 5

;; Evaluation rules define the behavior of eval:
;; * Evaluation Rule for Numbers, T, NIL : Numbers, and the symbols T and NIL are
;;   evaluated to themselves.
;;
;; * Evaluation Rule for Lists: The first element of the list specifies a function
;;   to be called. The remaining elements specify arguments to the function. The
;;   function is called on the evaluated arguments.

;; Defining Functions in Eval Notation
;; -----------------------------------
(defun average (x y)
  (/ (+ x y) 2.0))

;; defun is a special kind of function, called a "macro function", that does
;; not evaluate its arguments. Therefore they do not have to be quoted.
;; The first input to "defun" is the name of the function being defined.
;; The second input is the argument list: it specifies the names the function
;; will use to refer to its arguments.
;; The remaining inputs to "defun" define the body of the function: what goes
;; inside the box.
;; "defun" stands for "DEfine FUNction"

;; Another example:
(defun square (n)
  (* n n))

;; Almost any symbol except T or NIL can serve as the name of an argument.
;; Functions are more readable when their argument names mean something.
;; For example, a function that computed the total cost of a merchandise order
;; might name its arguments: quantity, price, and handling-charge.
(defun total-cost(quantity price handling-charge)
  (+ (* quantity price) handling-charge))

;; Excercises
;; 1. Write a function "pythag" that takes two inputs, x and y, and returns
;; the square root of x²+y². (pythag 3 4) should return 5.0
(defun pythag(x y)
  (sqrt (+ (* x x) (* y y))))

;; Variables
;; ---------
;; A variable is a place where data is stored. Variables are not symbols;
;; variables are named symbols. Functions are also named by symbols.
;; The value of a variable is the data it holds. The variables that are used
;; for the arguments of a function can only be referenced inside the body.
;; Outside of it, they are inaccessible.

;; Evaluating Symbols
;; ------------------
;; Evaluation Rule for Symbols: A symbol evaluates to the value of the variable
;; it refers to.

;; Using symbols and lists as data
;; -------------------------------
;; Suppose we want to call "equal" on the symbols kirk and spock. In eval notation
;; symbols are used to name variables, so if we write:
;; (equal kirk spock)
;; Lisp will think we are trying to compare the value of the global variable named
;; kirk with the value of the global variable named spock. Since we haven't given
;; any values to these variables, this will cause an error.
;; (equal kirk spock)  => Error! kirk unassigned variable.
;;
;; What we really want to do is compare the symbols themselves. We can tell Lisp to
;; treat kirk and spock as data, rather than as variable references by putting a quote
;; before each one.
;; (equal 'kirk 'spock)  => NIL
;;
;; The quote is short-hand for the function "quote". Therefore, (quote kirk) is the
;; same as 'kirk.
;; Because the symbols T and NIL evaluate to themselves, they don't need to be quoted
;; to use them as data. Most other symbols do, though:
;;(list 'james t 'kirk) => (james t kirk)

;; Whether symbols are used as data in a function definition, or are passed as inputs
;; when the function is called, depends on whether they are quoted or not:
(defun riddle(x y)
  (list 'why 'is 'a x 'like 'a y))

;; (riddle 'raven 4) => (why is a raven like a 4)

;; Lists also need to be quoted to use them as data; otherwise Lisp will try to
;; evaluate them, which typically results in an "undefined function" error.
;; (first (we hold these truths))  => *** - EVAL: undefined function WE
;; (first '(we hold these truths)) => we

;; Evaluation Rule for Quoted Objects: A quoted object evaluates to the object
;; itself, without the quote.

;; The Problem of Misquoting
;; -------------------------
;; It is easy for beginning Lisp programmer to get confused about quoting and
;; either put quotes in the wrong place or leave them out where they are needed.
