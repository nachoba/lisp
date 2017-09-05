#|
 
    filename:    lesson-001.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        04/09/2017
    revision:    05/09/2017
    description: Chapter 2 of "Land of Lisp".
                 This chapter introduces Lisp by programming a little
                 game in which we have to choose a number and guess it.

    contents:    [1] Global Variables
                 [2] Global Functions
                 [3] Ash
                 [4] Returning Values
                 [5] Changing the Values of Variables
                 [6] Local Variables
                 [7] Local Functions
                 [8] Summary

|#

(defparameter *small* 1)      ;; The lowes number in the guessing game.
(defparameter *big* 100)      ;; The highest number in the guessing game.

#|
    [1] GLOBAL VARIABLES
    A variable  that is defined globally is called a top-level-definition.
    The function "defparameter" is a bit confusing, since it does not
    really have anything to do with parameters. What is does is let you
    define global variables.

    (defparameter <name> <value>)

    The asterisks surrounding the names (called "earmuffs") are used by
    lispers to mark global variables by convention.

    Another function to declare global variables is "defvar". But is more
    appropriate to define global constants as values can not be over
    written unless the "setf" functions is used.

|#

(defun guess-my-number ()       ;; Using ash we are calculating the
  (ash (+ *small* *big*) -1))   ;; average of the sum of both variables.

#|

    [2] GLOBAL FUNCTIONS
    In Common Lisp, functions are defined with "defun", like this:

    (defun <function-name> (arguments)
      ...)

    If the function does not require parameters then we just put an
    empty parentheses ().

    Every command in Common Lisp generates a return value. The "defun"
    command, for instance, simply returns the name of the newly created
    function.
    
    [3] ASH
    The function "ash" is used to perform arithmetic shifting. This 
    function looks at a number in binary form, and then shifts its binary
    bits to the left or to the right, dropping any bits lost in the 
    process.
    We shift bits to the left using 1 as the second argument, and to the
    right using -1.

    (ash 11 1)    => In binary 11 is 1011, shifting to the left we get
                     10110, which is 22 in decimal.

    (ash 11 -1)   => In binary 11 is 1011, shifting to the right we get
                     0101, which is 5 in decimal.

    The function "ash" is commonly used in binary searches in Common Lisp.

    [4] RETURNING VALUES
    When programming in Lisp, you'll write many functions that won't
    explicitly print values on the screen. Instead, they'll simply return
    the value calculated in the body of the function. In some programming
    languages you have to write "return" to cause a value to be returned.
    This is not necessary in Lisp as the final value calculated in the
    body of the function is returned automatically.

|#

(defun smaller ()                       ;; Decrease by 1 the new value
  (setf *big* (1- (guess-my-number)))   ;; of the upper limit.
  (guess-my-number))

(defun bigger ()                        ;; Increase by 1 the new value 
  (setf *small* (1+ (guess-my-number))) ;; of the lower limit.
  (guess-my-number))

#|

    [5] CHANGING THE VALUE OF VARIABLES
    We use the function "setf" to change the value of our global variables.
    The "setf" function has the following syntax:

    (setf <variable-name> <new-value>)

    In these cases we used two additional functions "1-" and "1+" which
    subtract and add 1 to the argument.

|#

(defun start-over ()            ;; This function resets the values to
  (defparameter *small* 1)      ;; the original ones. And starts the
  (defparameter *big* 100)      ;; game again.
  (guess-my-number))




#|

    [6] DEFINING LOCAL VARIABLES
    When you want to limit your definitions to a single function or a
    block of code, you use local variables and functions.

    To define a local variable, use the command "let". A "let" command
    has the following structure:

    (let (variable declarations)
      ...body...)

    The first thing inside the "let" command is a list of variable
    declarations. This is where we declare one or more local variables.

    The in the body of the command (and only within this body), we can
    use this variables.

    When using a "let" expression, you must surround the entire list of
    declared variables with parentheses. Also, you must surround each
    pair of variable names and initial variables with another set of
    parentheses.

|#

(defun local-variable ()     ;; We define a function with no arguments.
  (let ((a 5)                ;; We define two local variables using let.
	(b 6))               ;; We add the values of the variables, but
    (+ a b)))                ;; outside the function they are not defined.


#|

    [7] DEFINING LOCAL FUNCTIONS
    We define local functions using the "flet" command, it has the 
    following structure:

    (flet ((function-name (arguments)
             ...function body...))
      ...body...)

    At the top of the "flet", we declare a function (in the first two
    lines). This function will then be available to us in the body.
    A function declaration consists of a function name, the arguments to
    that function, and the function body -where we put the function's
    code.
 
|#

(defun local-function ()
  (flet ((f (n)
	   (+ n 10)))
    (f 5)))

#|

    We define a single function, f, which takes a single argument, n.
    The function f then adds 10 to this variable n, which has been passed
    in it. 
    Then we call this function with the number 5 as the argument.

    You can define one or more functions within the scope of "flet".
|#

(defun local-functions ()
  (flet ((f (n)
	   (+ n 10))
	 (g (n)
	   (- n 3)))
    (g (f 5))))

#|

    Here we have declared two functions: one named "f" and another "g".
    In the body of the "flet", we can then refer to both functions.

    To make function names available in defined functions, we can use the
    "labels" command. It is identical in its basic structure to the "flet"
    command.

|#

(defun local-functions-callable ()
  (labels ((a (n)                   ;; We define the "a" function.
	     (+ n 5))
	   (b (n)                   ;; We define the "b" function, which
	     (+ (a n) 6)))          ;; calls the function "a".
    (b 10)))                        ;; We call the "b" function.


#|

    In this example, the local function "a" adds 5 to a number. Next,
    the function "b" is declared. It calls the function a, and then
    adds 6 to the result.
    Finally, the function "b" is called with the value 10.

    The special step that requires us to use "labels" instead of "flet"
    is where the function "b" calls the function "a". If we had used
    "flet", the function "b" would not have "known" about the function
    "a".

    The "labels" command lets you call one local function from another,
    and it allows you to have a function call itself. 
    This is commonly done in Lisp code and is called "recursion".


    [8] SUMMARY
    
    * To define a global variable, use the "defparameter" command.
    * To define a global function, use the "defun" command.
    * Use the "let" and "flet" commands to define local variables and
      functions.
    * The function "labels" is like "flet", but it lets functions call
      themselves. Functions that call themselves are called "recursive"
      functions.

|#
