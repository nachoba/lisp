#|
Functions as Objects
--------------------
In Lisp, functions are regular objects, like symbols or strings or lists. If we give
the name of a function to "function", it will return the associated object. Like
"quote", "function" is a special operator, so we don't have to quote the argument:

> (function +)
#<FUNCTION +>

This abbreviation is known as "sharp-quote".

Like any other kind of object, we can pass functions as arguments. One function that
takes a function as an argument is "apply". It takes a function and a list of arguments
for it, and returns the result of applying the function to the arguments:

> (apply #'+ '(1 2 3))
6

It can be given any number of arguments, so long as the last is a list:

> (apply #'+ 1 2 '(3 4 5))
15

The function "funcall" does the same thing but does not need the arguments to be
packaged in a list:

> (funcall #'+ 1 2 3)
6

|#
