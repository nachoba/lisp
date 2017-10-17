#|

    FILENAME:              005-functions.lisp
    AUTHOR:                Ignacio Sniechowski
    DATE:                  13/10/2017
    REVISION:
    DESCRIPTION:           From Chapter 5 of "Practical Common Lisp"
    SUMMARY:               [01] Introduction
                           [02] Defining New Functions
                           [03] Function Parameter Lists
                           [04] Optional Parameters
                           [05] Rest Parameters
                           [06] Keyword Parameters
                           [07]  


    CHAPTER 5 :: FUNCTIONS
    ----------------------
 
    [01] INTRODUCTION
    -----------------
    After the rules of syntax and semantics, the three most basic components of
    all Lisp programs are: functions, variables, and macros.
    We have used all three in the previous lesson but covered them in a basic
    manner. In the following chapters we will cover them in more detail.

    The bulk of Lisp itself consists of functions:

    * More than 3/4 of the names defined in the CL Standard are functions.
    * All the built-in data types are defined purely in terms of what functions
      operate on them.
    * Even Lisp's powerful object system (CLOS) is built upon a conceptual
      extension to functions, and generic functions.
    * Also, despite the importance of macros, in the end all real functionality
      is provided by functions. Remember that macros run at compile time, so the
      code they generate (the code that actually make up the program after all
      the macros are expanded) will consist entirely of calls to functions and
      special operators. Not to mention that macros themselves are also functions
      albeit functions that are used to generate code rather than to perform the
      actions of the program.

    [02] DEFINING NEW FUNCTIONS
    ---------------------------
    Normally functions are defined using the "defun" macro. The basic skeleton
    of a "defun" looks like this:

    (defun name (parameter*)
      "Optional documentation string."
      body-form*)

    Any symbol can be used as a function name; usually names contain only 
    alphabetical characters and hyphens, but other characters are allowed and
    are used in certain naming conventions.

    For instance, functions that convert one kind of value to another sometimes
    use "->" in the name.

    A function's parameter list defines the variables that will be used to hold
    the arguments passed to the function when it's called. If the function takes
    no arguments, the list is empty, written as ().
    Different flavors of parameters handle required, optional, multiple, and
    keyword arguments.

    If a string literal follows the parameter list, it's a documentation string
    that should describe the purpose of the function. When the function is
    defined, the documentation string will be associated with the name of the
    function and can later be obtained using the "documentation" function.

    Finally, the body of a "defun" consists of any number of Lisp expressions.
    They will be evaluated in order when the function is called and the value
    of the last expression is returned as the value of the function.
    Or the "return-form" special operator can be used to return immediately from
    anywhere in a function

    Let's analyze the parts of the following function:

    > (defun hello-world ()
        (format t "hello, world"))

    The name of the function is "hello-world", its parameter list is empty so it
    takes no arguments, it has no documentation string, and its body consists of
    one expression:
  
    (format t "hello, world")

    The following is a slightly more complex function:

|#

(defun verbose-sum (x y)
  "Sum any two numbers after priting a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

#|
    This function is named "verbose-sum", takes two arguments that will be bound
    to the parameters x and y, has a documentation string, and has a body consisting
    of two expressions.
    The value returned by the call to "+" becomes the return value of "verbose-sum".

    The documentation string can be accessed by evaluating:

    > (documentation 'foo 'function)


    [03] FUNCTION PARAMETER LISTS
    -----------------------------
    There's not a lot more to say about function names or documentation strings,
    and it will take a good portion of the rest of this book to describe all the
    things you can do in the body of a function, which leaves us with the parameter
    list.
  
    The basic purpose of a parameter list is, of course, to declare the variables
    that will receive the arguments passed to the function. When a parameter list 
    is a simple list of variable names -as in "verbose-sum"- the parameters are
    called required parameters.

    When a function is called, it must be supplied with one arguement for every
    required parameter. Each parameter is bound to the corresponding argument. If a
    function is called with too few or too many arguments, Lisp will signal an error.

    Common Lisp's parameter lists also give you more flexible ways of mapping the
    arguments in a function call to the function's parameters. In addition to required
    parameters, a function can have optional parameters. Or a function can have a 
    single parameter that's bound to a list containing any extra arguments.

    And, finally arguments can be mapped to parameters using keywords rather than
    position.


    [04] OPTIONAL PARAMETERS
    ------------------------
    Not all functions are so simple as to need only required parameters. Sometimes a
    function will have a parameter that only certain callers will care about, perhaps
    because there's a reasonable default value.

    An example is a function that creates a data structure that can grow as needed.
    Since that data structure can grow, it doesn't matter what the initial size is.
    But caler who have a good idea how many items they're going to put into the data
    structure may be able to improve performance by specifying a specific initial 
    size. Most callers, though, would probably rather let the code that implements
    the data structure pick a good general-purpose value.

    In Common Lisp you can accomodate both kinds of callers by using an optional
    parameter; callers who don't care will get a reasonable default, and other callers
    can provide a specific value.

    To define a function with optional parameters, after the names of any required
    parameters, place the symbol "&optional" followed by the names of the optional
    parameters. A simple example looks like this:
    
|#

(defun fooptional (a b &optional c d)
  "fooptional needs two required parameters and two optional parameters"
  (list a b c d))

#|

    When the function is called, arguments are first bound to the required parameters.
    After all the required parameters have been given values, if there are any arguments
    left, their values are assigned to the optional parameters.
    If the arguments run out before the optional parameters do, the remaining optional
    parameters are bound to the value "NIL".
    Thus, the function defined previously gives the following results:


    > (fooptional 1 2)
    (1 2 NIL NIL)
  
    > (fooptional 1 2 3)
    (1 2 3 NIL)

    > (fooptional 1 2 3 4)
    (1 2 3 4)

    Note, however, that Lisp still checks that an appropriate number of arguments are 
    passed to the function -in this case between two and four, inclusive- and will
    signal an error if the function is called with too few or too many.

    Of course, we'll often want a different default value than "NIL". We can specify
    the default value by replacing the parameter name with a list containing a name and
    an expression.
    The expression will be evaluated only if the caller doesn't pass enough arguments
    to provide a value for the optional parameter. The common case is simply to provide
    a value as the expression.

|#

(defun fooptional-default (a &optional (b 10))
  (list a b))

#|

    This function requires one argument that will be bound to the parameter "a". The
    second paramater, "b", will take either the value of the second argument, if there
    is one, or 10.

    > (fooptional-default 1 2)
    (1 2)

    > (fooptional-default 1)
    (1 10)


    Sometimes, however, you may need more flexibility in choosing the default value.
    You may want to compute a default value based on other parameters. And you can; the
    default value expression can refer to parameters that occur earlier in the
    parameter list.
    If you were writing a function that returned some sort of representation of a
    rectangle and you wanted to make it specially convenient to make squares, you might
    use and argument list like this:

|#

(defun make-rectangle (width &optional (height width))
  (list width height))

#|

    which would cause the height parameter to take the same value as the width parameter 
    unless explicitly specified.

    Occasionally, it's useful to know whether the value of an optional argument was
    supplied by the caller or is the default value. Rather than writing code to check
    whether the value of the parameter is the default (which doesn't work anyway, if the
    caller happens to explicitly pass the default value), you can add another variable
    name to the parameter specifier after the default-value expression.

    This variable will be bound to true if the caller actually supplied an argument for
    this parameter and "NIL" otherwise. By convenion, these variables are usually named
    the same as the actual parameter with a "-supplied-p" on the end. For example:

|#

(defun foonto (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

#|

    This gives results like this:

    > (foonto 1 2)
    (1 2 3 NIL)

    > (foonto 1 2 3)
    (1 2 3 T)

    > (fontoo 1 2 4)
    (1 2 4 T)


    [05] REST PARAMETERS
    --------------------
    Optional parameters are just the thing when you have discrete parameters for which
    the caller may or may not want to provide values. But some functions need to take
    a variable number of arguments.

    Several of the built-in functions you've seen already work this way. For instance, 
    "format" has two required arguments, the stream and the control string. But after
    that it needs a variable number of arguments depending on how many values need to
    be interpolated into the control string.

    The "+" function also takes a variable number of arguments -there's no particular
    reason to limit it to summing just two numbers; it will sum any number of values.
    (It even works with zero arguments, returning 0, the identity under addition).


    > (format t "hello, world")
    > (format t "hello, ~a" name)
    > (format t "x: ~d y: ~d" x y)
    > (+)
    > (+ 1)
    > (+ 1 2)
    > (+ 1 2 3)

    Those are all legal calls of those functions.

    You could write functions taking a variable number of arguments by simply giving
    them a lot of optional parameters. But that would be incredibly painful (just
    writing the parameter list would be bad enough). To do it properly, you'd have to
    have as many optional parameters as the number of arguemnts that can legally be
    passed in a function call. 
    This number is implementation dependent but guaranteed to be at least 50 (and in
    current implementations it is really hughe).

    Instead, Lisp lets you include a catch all parameter after the symbol "&rest". If
    a function includes a "&rest" parameter, any arguments remaining after the values
    have been doled out to all the required and optional parameters are gathered up
    into a list that becomes the value of the "&rest" parameter.

    Thus, the parameter list for "format" and "+" probably look something like this:

    (defun format (stream string &rest values) ...)
    (defun + (&rest numbers) ...)



    [06] KEYWORD PARAMETERS
    -----------------------
    Optional and rest parameters give you quite a bit of flexibility, but neither is
    going to help you out much in the following situation: suppose you have a function
    that takes four optional parameters.
    Now suppose that most of the places the function is caled, the caller wants to
    provide a value for only one of the four parameters and, further, that the callers
    are evenly divided as to which parameter they will use.


    The caller who want to provide a value for the first parameter are fine -they just
    pass the one optional argument and leave off the rest. But all the other callers
    have to pass some value for between one and three arguments they don't care about.
    Isn't that exactly the problem optional parameters were designed to solve?

    Of course it is. The problem is that optional parameters are still positional (if
    the caller wants to pass an explicit value for the fourth optional parameter, it 
    turns the first three optional parameters into required ones for that caller.

    Luckily, another parameter flavor, keyword parameters, allow the caller to specify
    which values go with which parameters.

    To give a function keyword parameters, after any required, "&optional", and "&rest"
    parameters you include the symbol "&key" and then any number of keyword parameter
    specifiers, which work like optional parameter specifiers.

    Heres a function that has only keyword parameters:
    
|#

(defun foo-keyword (&key a b c)
  (list a b c))

#|

    When this function is called, each keyword parameters is bound to the value 
    immediately following a keyword of the same name. Recall that keywords are names
    that start with a colon and that they're automatically defined as self-evaluating
    constants.

    If a given keyword doesn't appear in the argument list, then the corresponding
    parameter is assigned its default value, just like an optional parameter. Because
    the keyword arguments are labeled, they can be passed in any order as long as they
    follow any required arguments. For instance, "foo-keyword" can be invoked as:

    > (foo-keyword)
    (NIL NIL NIL)

    > (foo-keyword :a 1)
    (1 NIL NIL)

    > (foo-keyword :b 1)
    (NIL 1 NIL)

    > (foo-keyword :a 1 :c 3 :b 2)
    (1 2 3)


    As with optional parameters, keyword parameters can provide a default value form 
    and the name of a supplied-p variable. In both keyword and optional parameters, the
    default value form can refer to parameters that appear earlier in the parameter list.

|#

(defun foo-keyword2 (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))



