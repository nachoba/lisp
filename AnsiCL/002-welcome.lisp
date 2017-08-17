#|
Chapter 02 :: Welcome to Lisp
----------

Form
----
Any Lisp system will include an interactive front-end called the toplevel.
You type Lisp  expression into the toplevel, and the system displays their
values.

Number in Lisp evaluate to themselves, so, for example:

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


