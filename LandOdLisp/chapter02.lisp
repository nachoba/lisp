#| 
CHAPTER 02 :: Creating Your First Lisp Program
----------

The Guess-My-Number Game
------------------------
In this game, you pick a number from 1 to 100, and the computer has to guess it.
To create the game, we need to write three functions:  * guess-my-number
                                                       * smaller
                                                       * bigger                                             The player simply calls these functions from the REPL. 
Let's think about the strategy behind this simple game:
1. Determine the upper and lower (big and small) limit of the player's number.
   Since the range is between 1 and 100, the smallest possible number would be
   1 and the biggest would be 100.
2. Guess a number in between these two numbers.
3. If the player says the number is smaller, lower the big limit.
4. If the player says the number is bigger, raise the small limit.

By following these simple steps, cutting the range of possible numbers in half
every guess, the computer can quickly get the number. This type of search is
called "binary search".

Defining Global Variables in Lisp
---------------------------------
In order to track the small and big limits we'll need to create two global
variables called *small* and *big*. By convention, lispers like to mark all
their global variables with asterisks surrounding the names (called earmuffs).
A variable that is defined globally in Lisp is called a top-level-definition.
We can create new top-level definitions with the "defparameter" function.
There another command to define global variables called "defvar". The difference
is that values defined in this way can not be overwritten. They act more like
constants.
|#

(defparameter *small* 1)
(defparameter *big* 100)

#|
Defining Global Functions in Lisp
---------------------------------
The first function we will define is "guess-my-number". This function uses
the values of the *big* and *small* variables to generate a guess of the
player's number.
|#

(defun guess-my-number()
  (ash (+ *small* *big*) -1))

#|
The empty parentheses after the function name indicate that this function
doesn't require any parameters. What does this function do? The computer's
best guess in this game will be a number in between the two limits. To
accomplish this, we choose the average of the two limits. However, if the
average number ends up being a fraction, we'll want to use a near-average
number, since we're guessing only whole numbers.
We implement this by:
1. Adding the numbers that represent the high and low limits.
2. Using the arithmetic shift function "ash", to halve the sum of the limits
   and shorten the result. The built-in function ash looks at a number in
   binary form, and then shifts its binary bits to the left or right, dropping
   any bits lost in the process. By using "ash", we are continually halving
   our search space of possible numbers to quickly narrow down to the final
   correct number.

Defining the smaller and bigger Functions
-----------------------------------------
|#

(defun smaller()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

#|
We use the "setf" function to change the value of our global variable. The
code (1- (guess-my-number)) => first calls our guess-my-number function to
get the most recent guess, and then it uses the function "1-", which subtracts
1 from the result.

Defining the start-over Function
--------------------------------
To complete our game, we'll add the function "start-over" to reset our
global variables. This function resets the values of *small* and *big*
and then call "guess-my-number" again to return a new starting guess.
|#

(defun start-over()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))


#| 
Defining Local Variables in Lisp
--------------------------------
For our simple game, we've defined global variables an functions. However,
in most cases you'll want to limit your definitions to a single function
or a block of code. These are called "local variables" and functions.
To define a local variable, use the command "let", which has the following
structure:

(let (variable declarations)
   ... body ...)

For example:

(let ( (a 5) (b 6)) (+ a b))    => 11

When using a let expression, you must surround the entire list of declared
variables with parentheses. Also, you must surround each pair of variable
names and initial variables with another set of parentheses.

Note: Because the names of the variables and their values in a let expression
      form a kind of simple table, is a common practice to align the declared
      variables vertically.

(let ((a 5)
      (b 6))
  (+ a b))

Defining Local Functions in Lisp
--------------------------------
