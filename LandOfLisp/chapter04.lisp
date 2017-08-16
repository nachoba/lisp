#|
CHAPTER 04 :: Making Decisions with Conditions
----------
In this chapter, we'll be looking in detail at commands for handling conditions.
The elegance of these commands shows that the unusual philosophy and design of
Lisp has real practical benefits.

The Symmetry of "nil" and "()"
------------------------------
One thing is particularly striking when we look at how Lisp commands and data
structures work: They are imbued with symmetry in every conceivable way. This
symmetry can give your Lisp code a certain elegance that other languages cannot
have, and Lisp's simple syntax is an important factor in making this symmetry
possible.

Empty Equals False
------------------
Since the Lisp philosophy strongly emphasizes the use of lists to store and
manipulate information, it will come as no surprise that the design of CL,
flavors behaviors that make it easy to slice and dice such lists.
The most profound design decision made in Common Lisp, with regard to lists,
is that it automatically treats an empty list as a false value when evaluating
a condition:

> (if '()
      'i-am-true
      'i-am-false)

I-AM-FALSE

> (if '(1)
      'i-am-true
      'i-am-false)

I-AM-TRUE

Because we can easily detect and empty list, we can process lists using recursion.
With this technique, we can take items from the front of a list and send the rest
of the list back to the same function until the list is empty.

Let's look at a common recursive function which calculates the length of a list:
|#

(defun my-length (list)
  (if list
      (+ 1 (my-length (cdr list)))
      0))

#|
The Four Disguses of ()
-----------------------
Not only does the empty list evaluate to false, but it is the only false value in 
Common Lisp. Any value not equivalent to an empty list will be considered a true
value. All the following expressions are equivalente and are different disguises for
the one and only empty list:

() = '() = nil = 'nil

The Conditionals: if and Beyond
-------------------------------
Now that we understand how Lisp handles true and false, let's look at if and some
of the other conditional commands.

One Thing at a Time with if
---------------------------
The "if" command can be used to make different things happen when things are true
or false.

> (if (= (+ 1 2) 3)
      'yup
      'nope)

YUP

> (if (= (+ 1 2) 4)
      'yup
      'nope)

NOPE

So far, the only way to branch on a condition that we've looked at has been the if
command.

> (if (oddp 5)
      'odd-number
      'even-number)

ODD-NUMBER

All we do here is checking whether the number 5 is odd, then, depending on the result,
evaluating one of the two following expressionsin the if form. There are two important
observations to be made:
 * Only one of the expressions after the if is actually evaluated.
 * We can only do one thing in an if statement.

And to illustrate this this expression will evaluate:

> (if (oddp 5)
      'odd-number
      (/ 1 0))

ODD-NUMBER

Since only one expression inside an if es ever evaluated, it's impossible to do two or
more separate things inside your branch. However, for cases when you really want to do
more than one thing, you can use a special command "progn", to wedge in extra commands
in a single expression. With "progn" only the last evaluation is returned as the value
of the full expression.
|#

(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
	   'odd-number)
  'even-number)
;; Evaluates to "T"

#|
Going Beyond if: The when and unless Alternatives
-------------------------------------------------
Since it's kind of pain to use "progn" every time you want to do multiple things inside
and if, Lisp has several other commands that include an implicit "progn". The most ba-
sic of these are "when" and "unless":

With "when", all the enclosed expressions are evaluated when the condition is true.
With "unless", all the enclosed expressions are evaluated when the condition is false.
|#
(defvar *number-is-odd* nil)

(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)

;; Evaluates to "ODD-NUMBER"

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)

;; Evaluates to "EVEN-NUMBER"


#|
The trade-off is that these commands can't do anything when the condition evaluates in
the opposite way; they just return "nil" and do nothing.

The Command that Does it All: "cond"
------------------------------------
The "cond" form is the classic way to do branching in Lisp. Through the liberal use of
parentheses, it allows for an implicit "progn", can handle more than one branch, and
can even evaluate several conditions in succession.
|#

(defvar *arch-enemy* nil)

(defun pudding-eater (person)
  (cond ((eq person 'henry)  (setf *arch-enemy* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))
	((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
	 '(i hope you choked on my pudding johnny))
	(t
	 '(why you eat my pudding stranger?))))


#|
The conditions in a "cond" form are always checked from the top down, so the first
successful match drives the behavior. In this example, the last branch has a con-
dition of "t", guaranteeing that at least the last branch will always be evaluated.
This -catch all conditional- is a common "cond" idiom.

Branching with case
-------------------
Let's look at one final Lisp command: the "case" form. It is common to use the "eq"
for conditionals, and "case" lets you supply a value to compare against. Using "case",
we can rewrite the previous examples as follows, which is much easier to follow.
|#

(defun pudding-eater-two (person)
  (case person
	((henry)    (setf *arch-enemy* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))

	((johnny)   (setf *arch-enemy* 'useless-old-johnny)
	 '(i hioe you choked on my pudding johnny))

	(otherwise  '(why you eat my pudding stranger?))))

#|
Cool Tricks with Conditions
---------------------------
A couple of counterintuitive tricks involving conditions in Lisp can help you write
cleaner code.

Using the Stealth Conditionals "and" and "or"
---------------------------------------------
The conditionals "and" and "or" are simple mathematical operators, which allow you to
manipulate Boolean values in the same way you might manipulate numbers using addition
and subtraction.

To see if three numbers are odd:

> (and (oddp 5) (oddp 7) (oddp 9))

T

Similarly, we can use "or" to see whether at least one of a set of numbers is odd:

> (or (oddp 4) (oddp 7) (oddp 8))

T
But there's something more interesting about "and" and "or". These two commands look like
completely ordinary mathematical operators; they do not look like conditional commands, such
as "if" or "cond". However, they can be used for conditional behavior.
|#

;; Using "or" to set a global variable to true only when a number is even

(defparameter *is-it-even* nil)

(or (oddp 4) (setf *is-it-even* t))



