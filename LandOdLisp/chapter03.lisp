#|
CHAPTER 03 :: Exploring the Syntax of Lisp Code
----------
[1]Syntax & Semantics
---------------------
Lisp has a very simple syntax, so writing a Lisp compiler or interpreter is
easy. The part of a Lisp compiler or interpreter that reads in the code (which
lispers actually call the reader) is simpler than those of other programming
languages.
Lisp only has one way of organizing bits of code: It uses paretheses to organize
the code into lists. All basic Lisp code uses this simple list-like syntax:

(bla bla bla bla bla)

Inside a list we can put: other lists, symbols, numbers, and strings. These are
the basic datatypes you'll use in Lisp.

Symbols
-------
Symbols are a fundamental type of data in Lisp and are used extensively. A
symbol in Lisp is a stand-alone word. Symbols in Lisp are case-insensitive,
although most lispers avoid using uppercase). To illustrate this, we'll use
a function called "eq", which lets us see if two symbols are identical:

> (eq 'fooo 'FoOo)
T

As you can see, this function evaluates to T (true) which telss us that Lisp
considers these two symbols to be identical.

Numbers
-------
Lisp supports both floating-point numbers and integers. When you write a
number, the presence of a decimal point determines whether your number
is seen as floating-point number or an integer.
The numbers 1 and 1.0 are two different entities in Lisp.

Strings
-------
The last basic building block in Lisp is the string. To indicate a string, 
surround characters with double quotes. We can display a string using a
function called "princ"

> (princ "Hello world!")
Hello, world!

A string can also contain so-called "escaped characters", used if you want
to include double quotes or a backslash. For example:

> (princ "He yelled \"Stop that thief!\" from the busy street.")
He yelled "Stop that thief!" from the busy street.

[2]How Lisp Distinguishes Between Code and Data
-----------------------------------------------
When we write our Lisp program, how does Lisp decide which parts of our
program consist of code and which part are just data?
Lisp uses two modes when it reads your code: code mode & data mode.
You can switch between these two modes when writing Lisp code.

Code Mode
---------
Whenever you type something into the Lisp REPL, the compiler assumes that
you're entering a command you want to execute. In other words, Lisp always
assumes that you're writing code and defaults to code mode.
Lisp will except Lisp code to be entered as a list. However, the code 
should be in a special type of list: a "form". So when you're in code mode,
as you are when you start typing into the REPL, the commands you enter need
to be structured as forms.

A form is simply a list with a special command at the beginning -typically
the name of a function. When reading a form, Lisp sends all other items in
the list to the function as parameters.

(command bla bla bla bla bla)

For example:
> (expt 2 3)
8

The following example has two nested forms:

> (expt 2 (+ 3 4))
128

Data Mode
---------
As you might imagine, any stuff written in data mode is treated as data.
This means the computer will not try to "execute" it. Let's look at data
mode in action. We'll enter the same form that we entered before:

> '(expt 2 3)
(expt 2 3)

This time, we put a single quote in front of the list. Insted of responding
with the result, Lisp parrots our expression to us. The single quote tells
Lisp to treat the subsequent form as a chunk of data -simply a list of elements.
Lisp then prints the result of evaluating what we entered, which is the list
itself. It ignores any functions or variables in our list, treating everything
as data.
Placing a quote in front of lists so that they won't be evaluated as a command
is called "quoting". By using quoting, you can tell Lisp: "this next part
isn't a command. It's just a chunk of data for my program".

[3] Lists in Lisp
-----------------
Lists are a crucial feature of Lisp. They are what hold all you Lisp code (as
well as data) together. Take any basic piece of Lisp code, such as the following:

(expt 2 3)

This piece of code contains a symbol (expt) and two numbers, all tied together
as a list, indicated by the parentheses.
You can think of a Lisp program as a house: The wall will be made of lists.
The bricks will be made of symbols, numbers, and strings. What holds all
together is mortar, which in List terms are "cons cells".

Cons Cells
----------
List are held together with cons cells. Understanding the relationship
between cons cells and lists will give you a better idea of how Lisp
works.

A cons cell looks like this:

   -------------
   |     |     |
   -------------

It's made of two little connected boxes, both of which can point at other
things. A cons cell can point to another cons cell or another type of Lisp
data. By being able to point to two different things, it's possible to link
cons cells together into lists. In fact, lists are just an abstrac illusion,
all of them are actually composed of cons cells.

For instance, suppose we create the list '(1 2 3). Here's how this list is
represented in computer memory:

   -----------
   |    |    |
   -----------
     |     |
     1  -----------
        |    |    |
        -----------
          |     |
          2  -----------
             |    |    |
             -----------
               |     |
               3    NIL

It's created using three cons cells. Each cell points to a number, as well as
the next cons cell for the list. The final cons cell then points at NIL, to
terminate the list.

List Functions
--------------
Manipulating lists is extremely important in Lisp programming. There are three
basic functions for manipulating cons cells (and hence lists): * cons
                                                               * car
                                                               * cdr

The "cons" Function
------------------- 
If you want to link any two pieces of data in your Lisp
program (regardless of the type), the usual way to do that is with the cons
function. When you call "cons", the compiler typically allocates a small chunk
of memory, the cons cell, that can hold two references to the objects being
linked (usually the second of the two items being linked will be a list).
For example, let's link the symbol chicken to the symbol cat:

> (cons 'chicken 'cat)
(CHICKEN . CAT)

As you can see, cons returns a single object, the cons cell, represented by
parentheses and a dot between the two connected items. Don't confuse this with
a regular list. The dot in the middle makes this a cons cell, just linking those
two items together.
Notice how we prefix our two pieces of data with a single quote to make sure
that Lisp sees them as just data and doesn't try to evaluate them as code.

If instead of another piece of data, we attach the symbol "nil" on the right
side of the list, something special happens:

> (cons 'chicken 'nil) or (cons 'chicken nil)
(CHICKEN)

"nil" is a special symbol that is used to terminate a list. The lesson here is
that Lisp will always go out of its way to "hide" the cons cells form you.
When it can, it will show your results using lists. It will show a cons cell
(with the dot between the objects) only if there isn't a way to show your result
using lists. The previous example can be also written like this:

> (cons 'chicken ())
(CHICKEN)

The empty list (), can be used unterchangeably with the "nil" symbol in Lisp.
Thinking of the terminator of a list as an empty list makes sense; what do you
get when you add a chicken to an empty list? Just a list with a chicken in it.

The cons function can also add a new item to the front of the list. For example,
to add "pork" to the front of a list containing (beef chicken), use cons like:

> (cons 'pork '(beef chicken))
(PORK BEEF CHICKEN)

When lispers talk about using cons, they say they are "consing" something. In
this example, we "consed" pork to a list containing beef and chicken.

Since all lists are made of cons cells, our (beef chicken) list must have been
created from its own two cons cells, perhaps like this:

> (cons 'beef (cons 'chicken () ))
(BEEF CHICKEN)

Combining the previous two examples, we can see what all the lists look like
when viewed as "conses". This is what is really happening:

> (cons 'pork (cons 'beef (cons 'chicken ()) ) )
(PORK BEEF CHICKEN)

Basically, this is telling us that when we cons together a list of three items,
we get a list of three items. So, in Lisp, a chain of cons cells and a list are
exaclty the same thing.

The "car" and "cdr" Functions
-----------------------------
List are just long chains of two-item cells. The "car" function is used for getting
the thing out of the first slot of a cell:

> (car '(pork beef chicken))
PORK

The "cdr" function is used to grab the value out of the second slot, or the remainder
of a list:

> (cdr '(pork beef chicken))
(BEEF CHICKEN)

You can string together "car" and "cdr" into new functions like "cadr", "cdar", or
"cadadr". This lets you succintly extract specific pieces of data out of complex
lists. Entering "cadr" is the same as using "car" and "cdr" together -it returns
the second element form a list.

> (car (cdr '(pork beef chicken)))
BEEF

> (cadr '(pork beef chicken))
BEEF

The list Function
-----------------
For convenience, Lisp has many functions built on top of the basic three -cons, car,
and cdr. A useful one is the "list" function, which does the dirty work of creating
all the cons cells and builds our list all at once:

> (list 'pork 'beef 'chicken)
(PORK BEEF CHICKEN)

Remember that there is no difference between a list created with the "list" function,
one created by specifying individual cons cells, or one created in data mode using the
single quote. They are all the same. So:

> (cons 'pork (cons 'beef (cons 'chicken ()) ))
> (list 'pork 'beef 'chicken)
> '(pork beef chicken)

Are all the same.

Nested Lists
------------
Lists can contain other lists. For example:

> '(cat (duck bat) ant)
(CAT (DUCK BAT) ANT)

However, under the hood, these nested lists are still just made out of cons cells.
Let's look at an example where we pull items out of nested lists. Here, the first
item is (peas carrots tomatoes) and the second item is (pork beef chicken):

> (car '( (peas carrots tomatoes) (pork beef chicken)))
(PEAS CARROTS TOMATOES)

> (cdr '(peas carrots tomatoes))
(CARROTS TOMATOES)

> (cdr (car '((peas carrots tomatoes) (pork beef chicken))))
(CARROTS TOMATOES)

> (cdar '((peas carrots tomatoes) (pork beef chicken)))
(CARROTS TOMATOES)

The cons cells allow us to create complex structures, and we use them here to
build a nested list. To prove that our nested list consists solely of cons cells,
here how we could create this nested list using only the cons command:

> (cons (cons 'peas (cons 'carrots (cons 'tomatoes ()))) 
        (cons (cons 'pork (cons 'beef (cons 'chicken ()))) ()))
((PEAS CARROTS TOMATOES) (PORK BEEF CHICKEN))

Here are some more examples of functions based on "car" and "cdr" that we could
use on our data structure:

> (cddr ''((peas carrots tomatoes) (pork beef chicken) duck))
(DUCK)

> (caddr ''((peas carrots tomatoes) (pork beef chicken) duck))
DUCK

> (cadadr ''((peas carrots tomatoes) (pork beef chicken) duck))
BEEF

[4] Summary
-----------
* Parentheses in Lisp are there to keep the amount of syntax to a minimum.
* Lists are created from cons cells.
* You can create lists by making cons with the "cons" command.
* You can inspect the pieces of a list with "car" and "cdr".
|#
