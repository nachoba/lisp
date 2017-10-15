#|

    FILENAME:              005-functions.lisp
    AUTHOR:                Ignacio Sniechowski
    DATE:                  13/10/2017
    REVISION:
    DESCRIPTION:           From Chapter 5 of "Practical Common Lisp"
    SUMMARY:               [01] Introduction

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

|#
