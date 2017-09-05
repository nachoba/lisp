#|
 
    filename:    lesson-006.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        05/09/2017
    revision:    
    description: Chapter 4 from "Land of Lisp"
                 Continue exploring conditionals.

    contents:    [1] The Conditionals: "if" and Beyond.
                 [2] Going Beyond: The "when" and "unless" Alternatives
                 [3] The Command that Does it All: "cond"
                 [4] Branching with "case"

|#


#|

    [1] THE CONDITIONALS: if AND BEYOND
    Now that we understand how Lisp handles true and false, we'll look at "if"
    and some other conditionals.

    The "if" command can be used to make different things happen when things
    are true or false.

    > (if (= (+ 1 2) 3)                  ;; Checks if 3 = 3
          'yup                           ;; return YUP if it is true
          'nope)                         ;; return NOPE if it is false

    YUP



    > (if (= (+ 1 2) 4)                  ;; Checks if 3 = 4
          'yup                           ;; returns YUP if it is true
          'nope)                         ;; returns NOPE if it is false

    NOPE

|#

(defun odd-or-even (x)
  (if (oddp x)
      'odd-number
      'even-number))

#|

    There's a lot happening in this harmless-looking little command. Here are
    two important observations:

    * Only one of the expressions after the "if" is actually evaluated.
    * We can only do one thing in an "if" statement.

    Usually, when a function is executed in Lisp, all the expressions after the
    function name are evaluated, before the function itself is evaluated.
    However "if" does not follow these rules.
    Consider this example:

    > (if (oddp 5)
        'odd-number
        (/ 1 0))

    ODD-NUMBER


    Any self-respecting, law-abiding Lisp function would generate and error if
    we try to run this code: we are dividing by zero!!
    But "if" is not just a function. It is a "special form", which gives it
    special privileges, such as the right to not evaluate all its parameters in
    the normal way.

    This makes sense, since the whole point of a condition is to run some stuff
    but not other stuff. In this case, "if" just ignores the division by zero.
    Conditional commands in Lisp are typically special forms.

    Since only one expression inside an "if" is ever evaluated, it's impossible
    to do two or more separate things inside your branch.
    For cases when you really want to do more than one thing, you can use a
    special command, "progn", to add in extra commands in a single expression.

    With "progn", only the last expression es returned as the value of the full
    expression.

|#

(defvar *number-was-odd* nil)

(defun is-odd (x)
  (if (oddp x)
      (progn (setf *number-was-odd* t)    ;; We set the value to true: t
	     'odd-number)                 ;; The last expression is returned.
      'even-number))                      ;; This evaluates if it is false.


#|

    If we evaluate (is-odd 5) we will get ODD-NUMBER, which is the last
    expression inside "progn". But if we evaluate (princ *number-was-odd*) we
    get T, which shows that the first expression also was evaluated.

    [2] GOING BEYOND if: THE when AND unless ALTERNATIVES
    Lisp has several other commands that include an implicit "progn". The most
    basic of these are "when" and "unless".

    > (unless (oddp 4)
              (setf *number-is-odd* nil)
              'even-number)
    
    EVEN-NUMBER

    With "unless" all the enclosed expressions are evaluated when the condition
    is false.


    > (when (oddp 5)
            (setf *number-is-odd* t)
            'odd-number)

    ODD-NUMBER

    With "when" all the enclosed expressions are evaluated when the condition 
    is true.
   
    
    The trade-off is that these commands can't do anything when the condition
    evaluates in the opposite way; they just return "nil" and do nothing.



    [3] THE COMMAND THAT DOES IT ALL: cond
    


|#

