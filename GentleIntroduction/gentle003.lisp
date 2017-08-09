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
;; The error messages Lisp gives are good hint about what went wrong.
;; An unassigned variable or undefined function error usuaylly indicates that a
;; quote was left out:
;; (list 'a 'b c)    =>  *** - SYSTEM::READ-EVAL-PRINT: variable C has no value
;;
;; On the other hand, wrong-type input errors or funny results may be an indication
;; that a quote was put in where it doesn't belong.
;; (+ 10 '(- 5 2))   =>  *** - +: (- 5 2) is not a number
;;
;; When we quote a list, the quote must go outside the list to prevent the list from
;; being evaluated. If we put the quote inside the list, eval will try to evaluate
;; the list and an error will result:
;; ('foo 'bar 'baz)  =>  *** - EVAL: 'FOO is not a function name; try using a symbol instead

;; Three Ways to Make Lists
;; ------------------------
;; We have three ways to make lists using eval notation:
;; * We can write the list out directly using a quote to prevent its evaluation:
;;   '(foo bar baz)
;;
;; * We can use LIST or CONS to build the list up from individual elements. If we use this
;;   method we must quote each argument to the function:
;;   (list 'foo 'bar 'baz)
;;   (cons 'foo '(bar baz))

;;   One advantage of building the list up from individual elements is that some of the
;;   elements can be computed rather than specified directly.
;;   (list 33 'squared 'is (* 33 33))

;; Exercises
;; ---------
;; 1. The following expression evaluate without errors. Write down the result:
;;    (cons 5 (list 6 7)) => (cons 5 (6 7)) => (5 6 7)
;;    (cons 5 '(list 6 7)) => (5 list 6 7)
;;    (list 3 'from 9 'gives (- 9 3)) => (3 from 9 gives 6)
;;    (+ (length '(1 foo 2 moo))
;;       (third '(1 foo 2 moo)))  => (+ 4 2) => 6
;;    (rest '(cons is short for construct)) => (is short for construct)
;;
;; 2. The following expression contain errors. Correct them.
;;    (third (the quick brown fox)) => (third '(the quick brown fox)) => (brown)
;;    (list 2 and 2 is 4)           => (list 2 'and 2 'is 4) => (2 and 2 is 4)
;;    (+ 1 '(length (list t t t t))) => (+ 1 (length (list t t t t) => 5
;;    (cons 'patrick (seymour marvin)) => (cons 'patrick '(seymour marvin))
;;    (cons 'patrick (list seymour marvin)) => (cons 'patrick (list 'seymour 'marvin)
;;
;; 3. Define a predicate "longer-than" that takes two lists as input and returns T if the
;;    first list is longer than the second:
(defun longer-than(a b)
  (> (length a) (length b)))

;; 4. Write a function "addlength" that takes a list as input and returns a new list
;;    with the length of the input added onto the fron of it.
(defun addlength (a)
  (cons (length a) a))

;; Four Ways to Misdefine a Function
;; ---------------------------------
;; Beginning users of eval notation sometimes have trouble writing syntactically correct
;; function definitions. Let's take a close look at a proper definition for the function
;; "intro":
(defun intro(x y)
  (list x 'this 'is y))

;; The first way to misdefine a function is to put something other than plain unadorned
;; symbols in the function's argument list. If we put quotes or extra levels of parentheses
;; in the argument list, the function won't work.
;; (defun intro ('x 'y) (list x 'this 'is y))  => Bad Argument List
;; (dufun intro ((x) (y) (list x 'this 'is y)) => Bad Argument List

;; The second way to misdefine a function is to put parentheses around the variables where they
;; appear in the body. Only function calls should have parentheses around them.
;; (defun intro(x y) (list (x) 'this 'is (y)))  => X undefined function

;; The third way to misdefine a function is to quote a variable. Symbols must be left unquoted
;; when they refer to variables.
;; (defun intro(x y) (list 'x 'this 'is 'y)) => (x this is y)

;; The fourth way is to not quote something that should be quoted.
;; (defun intro(x y) (list x this is y) => Error THIS unassigned variable


;; More About Variables
;; --------------------


;; Summary
;; -------
;; Lists are interpreted by the eval function according to a buit-in set of evaluation rules:
;; * Numbers are self-evaluating, meaning they evaluate to themselves. So do T and NIL.
;; * When evaluating a list, the first element specifies a function to call, and the remaining
;;   elements specify its arguments. The arguments are evaluated from left to right to derive the
;;   inputs that are passed to the function.
;; * Symbols appearing anywhere other than the first element of a list are interpreted as variable
;;   references. A symbol evaluates to the value of the variable it names.
;; * A quoted list or symbol evaluates to itself, whitout the quote.
;;
;; A list of form (defun function-name (argument-list) function-body) defines a function. Defun is a
;; special kind of function; its inputs do not have to be quoted. A function's argument list is a list
;; of symbols giving names to the function's inputs. Inside the body of the function, the variables
;; that hold the function's inputs can be referred to by these symbols.




