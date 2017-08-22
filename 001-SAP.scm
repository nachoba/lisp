#|
Scheme and the Art of Programming
---------------------------------

Part I :: Data
------
Data are either individual units of information or collections of data. 

Chapter 01 :: Data and Operators
----------
The foremost challenging of programmin is managing complexity, and abstraction is
the primary technique for managing complexity. An abstraction hides unnecessary
detail and allows recurring patterns to be expressed concisely. In programming,
less is more and simplicity is practical. It is essential that programs be intelligle
by humans as well as by computers. Programming teaches an algorithmic approach to
problem solving, which in turn encourages an algorithmic approach to gaining
knowledge.

A short bibliography about Scheme
---------------------------------
* "Structure and Interpretation of Computer Programs" by Abelson and Sussman, MIT.
* "The Little Schemer" by Friedman and Felleisen, MIT.
* "The Scheme Programming Language" by Dybvig, Prentice-Hall.
* "Programming in Scheme" by Eisenberg, Scientific Press.
* "An Introduction to Scheme" by Smith, Prentice-Hall.

Numbers and Symbols
-------------------
Computer languages have the analogs of words in the English language, they are called
symbols. All of the characters can be used for a symbol, except for

                ( )  [ ]  { }  ;  ,  "  '  `  #  \

which have special meaning. Similarly, the character

                + - .

are used in numbers and may occur anywhere in a symbol except as the first character.
Numbers are not considered to be symbols in Scheme; they form a separate category. A
symbol used to represent some value is called a variable. When Scheme it is given a
symbol; it checks to see if it is one of those that has been defined. If so, it can
use that meaning. Otherwise it tells us that the symbol has not yet been given a
name.
As words in English are put together to form sentences, in Scheme, symbols are used
to define expressions. An expression may consist of a single symbol or number, or a
list.
To define a symbol we use a "define" expression. In general, if <var> represents a
variable and <expr> represents an expression whose value we would like to bind to
<var>, we accomplish that by:

                (define <var> <expr>)

|#

(define ten 10)

#|
Now suppose that we want Scheme to print not the value 10 but insted print the symbol
ten. We quote a symbol by enclosing in parentheses the word "quote" followed by the
symbol:

                (quote <symbol>)

Instead of typing "quote" we can use ' to quote a symbol

                'ten   => ten

We can also assign to a variable a value that is the literal value of a symbol:
|#

(define Robert 'Bob)

#|
We have two types of data so far: numbers and symbols. The use of numbers should be no surprise,
since we usually think of doing arithmetic operations on numbers. Sceheme uses prefix notation
whenever it applies any kind of operator to its operands.

Constructing Lists
------------------
It is convenient to keep information on lists and to be able to manipulate that information. A
list is denoted by a collection of items enclosed by parentheses. Scheme provides a procedure
to build lists one element at a time. This procedure is called "cons", a shortening of "constructor".

We can build a list that contains only the number 1, by consing 1 to the empty list ().

                (cons 1 '())   => (1)  

When Scheme sees an expression enclosed in parentheses, it assumes that the first item following the
left parenthesis evaluates to a procedure. It then evaluates the expression according to what the
first item tells it to do. So if we enter something like:

                (3 2)   => Error: bad procedure 3

Taking Lists Apart
------------------
We have seen how to build lists using the constructor "cons". We now consider how to take a list
apart so that we can manipulate the pieces separately and build new lists from old. We accomplish
this decomposition of lists using two selector procedures, "car" and "cdr".

If ls represents a nonempty list of items:

(car ls)   => gives the first element of ls
(cdr ls)   => gives the list consisting of all items in ls with the exception of its first item.

Both "car" and "cdr" take one argument that must evaluate to a nonempty list, and both are not
defined on an empty list, and applying them to an empty list produces an error.

With the three operators "car", "cdr", and "cons" we can do almost anything we want with lists.
|#

; get the second element of a list.
(define (second list)
  (car (cdr list)))

; get the third element of a list.
(define (third list)
  (car (cdr (cdr list))))

#|
We see that the procedures "car" and "cdr" are applied in succession a number of times. The
successive applications of car's and cdr's is facilitated by the use of the procedures:
"caar", "cadr", "caddr", .... , "cddddr". The number of a's and d's between the "c" and the
"r" tells us how many times we apply "car" or "cdr" respectively.

So the two previous procedures can be rewritten as:
|#

(define (second2 list)
  (cadr list))

(define (third3 list)
  (caddr list))

#|
If the second argument of "cons" is not a list, we can still apply it, but the result is
not a list but rather a dotted pair. A dotted pair is written as a pair of objects,
separated by a dot (or period) and enclosed by parentheses. The first object in the dotted
pair is the "car" of the dotted pair, and the second object in the dotted pair is the "cdr"
of the dotted pair.

                (cons 'a 'b)   => (a . b)
                (car '(a . b)  => a
                (cdr '(a . b)  => b

Thus any item built with the constructor "cons" is referred as a pair.

                '(a . ())      => (a)
                '(a . (b c))   => (a b c)


Predicates
----------
Another group of procedures, called predicates, applies a test to their arguments and
returns true or false depending on whether the test is passed. Scheme uses #t to denote
true, and #f to denote false. These are called boolean (or logical) values. They form
a separate type of data type. So far we've seen the following data types: numbers,
symbols, pairs (including lists), procedures, and booleans.

                (number? <expr>)     => returns #t if <expr> is a number.
                (symbol? <expr>)     => returns #t if <expr> is a symbol.
                (boolean? <expr>)    => returns #t if <expr> is a boolean.
                (pair? <expr>)       => returns #t if <expr> is a pair.
                (null? <expr>)       => returns #t if <expr> is the empty list ().
                (procedure? <expr>)  => returns #t if <expr> is a procedure.

A question that we often ask is whether two objects are the same. Scheme offers several
different predicates to test for the sameness of its arguments. Which predicate you use
depends upon the information you seek and the data type of the objects.

When both objects are numbers, we use the predicate "=" to test whether its arguments
represent the same number:

                (= 3 (/ 6 2))        => #t

If we want to test the sameness of symbols, we use the predicate "eq?". Symbols have the
property that they are identical if they are written with the same characters in the
same order:

                (eq? 'cat 'cat)      => #t

When we want to include numbers, symbols, and booleans in the types of objects the
predicate tests for sameness, we use the predicate "eqv?", and if we want a universal
sameness predicate that can be applied to test numbers, symbols, booleans, procedures,
and lists (and strings, character, and vectors), we use the predicate "equal?".

Now the obvious question: How do we know which one to use? When a predicate must first
test to determine the type of its argument, it is less efficient than one designed 
specifically for the type of its arguments.

23aug17is
|#
