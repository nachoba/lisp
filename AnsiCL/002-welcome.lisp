#|
Chapter 02 :: Welcome to Lisp
----------

Form
----
Any Lisp system will include an interactive front-end called the toplevel.
You type Lisp  expression into the toplevel, and the system displays their
values.

Numbers in Lisp evaluate to themselves, so, for example:

> 1
1

> 15.456
15.456

In Lisp, expressions are entered with the operator (or function) first,and
then the arguments, all enclosed between parentheses.This is called prefix
notation, because the operator comes first. For example, 2 + 3 + 4 will be
entered, in Lisp as:

> (+ 2 3 4)
9

Because operators can take varying numbers of arguments, we need parenthe-
ses to show when an expression begins and ends. Expressions can be nested.


> (/ (- 7 1) (- 4 2))
> (/ (6) (2))
3

Another beauty of Lisp is: this is all there is.  All Lisp expressions are
either "atoms", like 1, or "lists",  which consist of zero or more expres-
sions enclosed in parentheses. All Lisp code takes this form.

Evaluation
----------
In this section we take a closer look at how expressions are evaluated. In
Lisp, + is a function, and an expression like  (+ 2 3) is a function call.
When Lisp evaluates a function call, it does so in two steps:

   1. First the arguments are evaluated, from left to right. In this case,
      each argument evaluates to itself,so the values of the arguments are
      2 and 3, respectively.
   2. The values of the arguments are passed to the  function named by the
      operator. In this case, it is the + function, which returns 5.

If any of the  arguments are themselves function calls, they are evaluated
according to the same rules. So when (/ (- 7 1) (- 4 2)) is evaluated,this
is what happens:

   1. Lisp evaluates (- 7 1). 7 evaluates to 7, and 1 evaluates to 1.These
      values are passed to the function -, which returns 6.
   2. Lisp evaluates (- 4 2). 4 evaluates to 4, and 2 evaluates to 2.These
      values are passed to the function -. which returns 2.
   3. The values 6 and 2 are sent to the function /, which returns 3.

Not all the operators in Lisp are  functions,  but most are.  And function
calls  are always evaluated  this way. The arguments are evaluated left to
right,and their values are passed to the function, which returns the value
of the expression as a whole. This is called the evaluation rule for Lisp.

--------------------------------------------------------------------------
Getting Out of Trouble
----------------------
If you type something that Lisp can't understand, it will display an error
message and put you into a  version of the toplevel called the break loop.
The break loop gives  experienced programmers  a chance to figure out what
caused the error,   but initially  the only thing you will want to do in a
break loop is get out of it. What you have to type to get back to the top-
level  depends on your implementation of Common Lisp. In this hypothetical
implementation :abort does it.

> (/ 1 0)
Error: Division by zero.
       Options :abort, :backtrace
>> :abort
>

--------------------------------------------------------------------------

One operator that does not follow the Lisp evaluation rule is "quote". The
"quote" operator is a special operator,  meaning that it has a distinct e-
valuation rule of its own. And the rule is: do nothing. The "quote" opera-
tor takes a single argument, and just returns it verbatim:

> (quote (+ 3 5))
(+ 3 5)

For convenience, Lisp defines ' as an abbreviation for "quote".You can get
the effect of calling quote by affixing a ' to the front of any expression

> '(+ 3 5)
(+ 3 5)

It is much more common to use the abbreviation than to write out the whole
"quote" expression.  Lisp provides  the "quote" as a way of protecting ex-
pressions from evaluation. 

Data
----
Lisp offers all the data types we find in most other languages, along with
several others that we do not.  One data type we  have used already is the
integer, which is written as a series of digits.  Another  data type  Lisp
has in common with most other languages is the string,  which is represen-
ted as a  series of characters surrounded  by double-quotes.  Integers and
strings both evaluate to themselves.
Two Lisp data types that we don't commonly find in other languages are the
lists and the symbols. Symbols are words, ordinarily they are converted to
uppercase, regardless of how you type them.

> 'Artichoke
ARTICHOKE

Symbols do not (usually) evaluate to themselves,so if you want to refer to
a symbol, you should quote it, as above.

Lists are represented as zero or more elements enclosed in parentheses.The
elements can be of any type, including lists. You have to quote lists,  or
Lisp would take them for function calls:

> '(my 3 "Sons")
(MY 3 "Sons")

> '(the list (a b c) has 3 elements)
(THE LIST (A B C) HAS 3 ELEMENTS)

Notice  that one quote protects a whole expression, including  expressions
within it. You can build lists by calling "list". Since "list" is a  func-
tion, its arguments are evaluated. Here we see a call to "+" within a call
to "list":

> (list 'my (+ 2 1) "Sons")
(MY 3 "Sons")

We are now in a position to appreciate one of the most remarkable features
of Lisp. Lisp programs are expressed as lists.If the arguments of flexibi-
lity and elegance did  not convince  you that Lisp notation  is a valuable
tool, this point should.It means that Lisp program can generate Lisp code.
Lisp programmers can write programs to write their programs for them.

It is important to understand the relation between expressions and  lists,
if only to avoid being confused by it. This is why we need the quote. If a
list is quoted, evaluation returns  the list itself;  if it is not quoted,
the list is treated as code, and evaluation returns its value:

> (list '(+ 2 1) (+ 2 1))
((+ 2 1) 3)

Here the first argument is quoted, and so yields a list.  The second argu-
ment is not quoted, and is treated as a function call, yielding  a number.
In Common Lisp, there are two ways of representing the empty list. You can
represent it as a pair of parentheses  with nothing between them,  or you
can use the symbol "nil". It does not matter which way you write the empty
list, but it will be displayed as "nil":

> ()
NIL

> nil
NIL

You do not have to quote "nil" (though you could) because nil evaluates to
itself.

List Operations
---------------
The function "cons" builds lists. If its second argument is a list, it re-
turns a new list with the first argument added to the front:

> (cons 'a '(b c d))
(A B C D)

We can build up lists by consing new elements onto an empty list with  the
"list" function that we saw in the previous section is just a more  conve-
nient way of consing several things onto "nil":

> (cons 'a (cons 'b nil))
(A B)

> (list 'a 'b)
(A B)

The primitive functions for extracting the elements of lists are "car" and
"cdr". The "car" of a list is  the first  element, and the "cdr" is every-
thing after the first element.

> (car '(a b c))
A

> (cdr '(a b c))
(B C)

You can use combinations of "car" and "cdr" to reach any element of a list
If you want to get the third element, you could say:

> (car (cdr (cdr '(a b c d))))
C

However, you can do the same thing more easily by calling "third":

> (third '(a b c d))
C

Truth
-----
In Common Lisp, the symbol "t" is the default representation for
truth. Like "nil", "t" evaluates to itself. The function "listp" returns
true if its argument is a list:

> (listp '(a b c))
T

A function whose return value is intended to be interpreted as truth  or
falsity is called a "predicate". Common Lisp predicates often have names
that end with "p". 
Falsity in Common Lisp is represented by "nil", the  empty list.   If we
give "listp" an argument that isn't a list, it returns "nil":

> (listp 27)
NIL

Because "nil" plays two roles in Common Lisp, the function "null", which
returns true of the empty list, and  the function "not",   which returns
true if its argument is false, do exactly the same thing.

> (null nil)
T

> (not nil)
T

The simplest conditional in Common Lisp is "if". It usually takes three
arguments: a test expression, a then expression, and an else expression.
The test expression is evaluated: if it returns true, the then expres-
sion is evaluated and its value is returned. If the test expression
returns false, the else expression is evaluated and its value is retur-
ned.

> (if (listp '(a b c))
      (+ 1 2)
      (+ 5 6))
3

> (if (listp 27)
      (+ 1 2)
      (+ 5 6))
11

Like quote, "if" is a special operator. It could not possibly be imple-
mented as a function, because the arguments in a function call are
always evaluated, and the whole point of "if" is that only one of the 
last two arguments is evaluated. The last argument to "if" is optional.
If you omit it, it defaults to "nil":

> (if (listp 27)
      (+ 2 3))
NIL

Although "t" is the default representation for truth, everything except
"nil" also counts as "true" in a logical context:

> (if 27 1 2)
1

Simply, "if" acts like this:
> (if <condition> <true> <false>)

The logical operators "and" and "or" resemble conditionals. Both take
any number of arguments, but only evaluate as many as they need in
order to decide what to return. If all its arguments are true (that is,
not nil), then and returns the value of the last one:

> (and t (+ 1 2))
3

But if one of the arguments turns out to be false, none of the argu-
ments after that get evaluated. Similarly for "or", which stops as
soon as it finds an argument that is true.

These two operators are "macros". Like special operators, macros can
circumvent the usual evaluation rule.

Functions
---------
You can define new functions with "defun". It usually takes three or
more arguments: a name, a list of parameters, and one or more expres-
sions that will make up the body of the function. Here is how we might
define "third":
|#

(defun my-third (x)
  (car (cdr (cdr x))))

#|
The first argument says that the name of this function will be:
"my-third". The second argument, the list (x), says that the function
will take exactly one argument: x. A symbol used as a placeholder in 
this way is called a "variable". When the variable represents an argu-
ment to a function, as x does, it is also called a parameter.
The rest of the definition, (car (cdr (cdr x))), is known as the body
of the function. It tells Lisp what it has to do to calculate the re-
turn value of the function. So a call to "my-third" returns the eva-
luation of the expression "(car (cdr (cdr x)))", for whatever x we
give as the argument:

> (my-third '(a b c d))
C

Now that we've seen variables, it's easier to understand what symbols
are. They are variable names, existing as objects in their own right.
And that's why symbols, like lists, have to be quoted. A list has to
be quoted because otherwise it will be treated as code; a symbol has
to be quoted because otherwise it will be treated as a variable.
You can think of a function definition as a generalized version of a
Lisp expression. The following expression tests whether the sum of 1
and 4 is greater than 3:

> (> (+ 1 4) 3)
T

By replacing these particular numbers with variables, we can write a
function that will test whether the sum of any two numbers is greater
than a third:
|#

(defun sum-greater (x y z)
  (> (+ x y) z))

#|

> (sum-greater 1 4 3)
T

Lisp makes no distinction between a program, a procedure, and a func-
tion. Functions do for everything (and indeed, make up most of the 
language itself). If you want to consider one of your functions as 
the main function, you can, but you will ordinarily be able to call
any function from the toplevel. Among other things, this means that
you will be able to test your programs piece by piece as you write
them.


Recursion
---------
The functions we defined in the previous section called other functions
to do some of their work for them. A function can call any function,
including itself. A function that calls itself is recursive.
The Common Lisp function "member" test whether something is an element
of a list. Here is a simplified version defined as a recursive function:
|#

(defun my-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (my-member obj (cdr lst)))))

#|
The predicate "eql" test whether its two arguments are identical; aside
from that, everything in this definition is something we have seen be-
fore. Here it is in action:

> (my-member 'b '(a b c))
(B C)

> (my-member 'z '(a b c ))
NIL

The definition of "my-member" corresponds to the following English
description. To test whether an object "obj" is a member of a list
"lst", we:
  1. First check whether "lst" is empty. If it is, then "obj" is
     clearly not a member of it, and we're done.
  2. Otherwise, it "obj" is the first element of "lst", it is a 
     member.
  3. Otherwise "obj" is only a member of "lst" if it is a member of
     the rest of "lst"

When you want to understand how a recursive function works, it can
help to translate it into a description of this kind.

Input and Output
----------------
For the real interactive programs this is not likely to be enough.
In this section we look at a few functions for input and output.
The most general output function in Common Lisp is "format". It
takes two or more arguments: the first indicates where the outputs
is to be printed, the second is a string template, and the remaining
arguments are usually objects whose printed representations are to
be inserted into the template. Here is a typical example:
|#

(format t "~a plus ~a equals ~a.~%" 2 3 (+ 2 3))

#|
2 plus 3 equal 5
T

Notice two things get displayed here. The first line is displayed by
"format". The second line is the value returned by the call to "format",
displayed in the usual way by the toplevel. Ordinarily a function like
"format" is not called directly from the toplevel, but used within
programs, so the return value is never seen.

The first argument to "format", "t", indicates that the output is to be
sent to the default place. Ordinarily this will be the toplevel. The
second argument is a string that serves as a template for output.
Within this string, each ~a indicates a position to be filled, and the
~% indicates a newline. The positions are filled by the values of the
remaining arguments, in order.

The standard function for input is "read". When given no arguments, it
reads from the default place, which will usually be the toplevel. Here
is a function that prompts the user for input, and returns whatever is
entered:
|#

(defun askem (string)
  (format t "~a ~%" string)
  (read))

#|
It behaves as follows:

> (askem "How old are you? ")
How old are you? 29
29

Bear in mind that "read" will sit waiting indefinitely until you type
something and (usually) hit return. So it's unwise to call "read" without
printing an explicit prompt, or your program may give the impression that
it is stuck, while in fact it's just waiting for input.

The second thing to know about "read" is that it is very powerful: "read"
is a complete Lisp parsers. It doesn't just read characters and return
them as a string. It parses what it reads, and returns the Lisp object
that results. In the case above, it returned a number.

Short as it is, the definition of "askem" shows something we haven't seen
before in a function. Its body contains more than one expression. The body
of a function can have any number of expressions. When the function is
called, they will be evaluated in order, and the function will return the
value of the last one.

In all the sections before this, we kept to what is called "pure Lisp",
that is, Lisp without side-effects. A side-effect is some change to the
state of the world that happens as a consequence of evaluating an
expression. When we evaluate a pure Lisp expression like (+ 1 2), there
are no side-effects; it just returns a value. But when we call "format",
as well as returning a value, it prints something. That's one kind of
side-effect.

When we are writing code without side-effects, there is no point in
defining functions with bodies of more than one expression. The value of
the last expression is returned as the value of the function, but the
values of any preceding expressions are thrown away. If such expressions
didn't have side-effects, you would have no way of telling whether Lisp
bothered to evaluate them all.

Variables
---------
One of the most frenquently used operators in Common Lisp is "let", which
allows you to introduce new local variables:

> (let ((x 1) (y 2))
     (+ x y))

3

A "let" expression has two parts: First comes a list of instructions for
creating variables, each of the form (variable expression). Each variable
will initally be set to the value of the corresponding expression. So in
the example above, we create two new variables, x and y, which are
initially set to 1 and 2, respectively. These variables are valid within
the body of the let.
After the list of variables and values comes a body of expressions, which
are evaluated in order. In this case there is only one, a call to +. The
value of the last expression is returned as the value of the let. Here is
an example of a more selective version of "askem" written using let:
|#

(defun ask-number ()
  (format t "Please enter a number: ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

#|
This function creates a variable "val" to hold the object returned by
"read". Because it has a handle on this object, the function can look at
what you entered before deciding whether or not to return it. As you
probably guessed, "numberp" is a predicate that test whether its
argument is a number. If the value entered by the user isn't a number,
"ask-number" calls itself. The result is a function that insists on getting
a number.

Variables like those we have seen so far are called local variables. They
are only valid within a certain context. There is another kind of variable,
called a global variable, that can be visible everywhere. You can create a
global variable by giving a symbol and a value to "defparameter":
|#

(defparameter *glob* 99)

#|
Such a variable will then be accessible everywhere, except in expressions
that create a new local variable with the same name. To avoid the
possibility of this happening by accident, it's conventional to give
global variables names that begin with asterisks. The name of the variable
we just created would be pronounced "star-glob-star".

You can also define global constants, by calling "defconstant":
|#

(defconstant limit (+ *glob* 1))

#|
There is no need to give constants a distinctive name, because it will
cause an error if anyone uses the same name for a variable. If you want to
check whether some symbol is the name of a global variable or constant,
use "boundp":

> (boundp '*glob*)
T

Assignment
----------
In Common Lisp the most general assignment operator is "setf". We can use
it to do assignments to either kind of variable:
|#

(setf *glob* 23)

(let ((n 10))
  (setf n 2)
  n)

#|
When the first argument to "setf" is a symbol that is not the name of a
local variable, it is taken to be a global variable:
|#

(setf x (list 'a 'b 'c))

#|
Will evaluate to: (A B C)
That is, you can create global variables implicitly, just by assigning
them values. In source files, at least, it is better style to use explicit
"defparameter".
You can do more than just assign values to variables. The first argument
to "setf" can be an expression as well as a variable name. In such cases,
the value of the second argument is inserted in the place referred to by
the first:

> (setf (car x) 'n)
N
> x
(N B C)

The first argument to "setf" can be almost any expression that refers to a
particular place. You can give any (even) number of arguments to "setf".
An expression of the form:
> (setf a b
        c d
        e f)

is equivalent to three separate calls to "setf" in sequence:

> (setf a b)
> (setf c d)
> (setf e f)


Functional Programming
----------------------


|#



