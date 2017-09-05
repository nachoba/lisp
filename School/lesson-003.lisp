#|
 
    filename:    lesson-003.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        05/09/2017
    revision:    
    description: Chapter 3 of "Land of Lisp"
                 In this lesson we will take a look at how Lisp
                 distinguishes between code and data.

    contents:    [1] Introduction
                 [2] Code Mode
                 [3] Data Mode
                 [4] Lists in Lisp
                 [5] Cons Cells

|#


#|

    [1] INTRODUCTION
    How does Lisp decide which parts of our program consist of code -stuff
    to be executed-, and which part are just data?
    The syntax of Lisp has a special way of distinguishing between the two.
    Common Lisp uses two modes when it reads your code: a "code mode" and a
    "data mode".

    You can switch between these two modes when writing Lisp code.


    [2] CODE MODE
    Whenever you type something into the Lisp REPL, the compiler assumes
    that you're entering a command you want to execute.
    Lisp always assumes that you're writing code and defaults to code mode.

    Lisp will expect Lisp code to be entered as a list. However, the code
    should be in a special type of list: "a form". So, in code mode, the
    commands you enter need to be structured as forms:

    (foo bla bla bla bla)

    A form is simply a list with a special command at the beginning.
    Typically the name of a function (in the above example, the function
    would be "foo").

    When reading a form, Lisp sends all other items in the list to the 
    function as parameters.
    When Lisp reads the text for the parameters of such a command, it
    usually assumes that these parameters are also in code mode:

    > (expt 2 (+ 3 4))
    128


    [3] DATA MODE
    Any stuff written in data mode is treated as data. This means the
    computer will not try to "execute" it, which allows us to have
    information in our code that's just plain data.

    If we put a single quote in front of the list, instead of responding
    with the evaluation of the expression, Lisp simply parrots our expression
    to us.

    The single quote tells Lisp to treat the subsequent form as a chunk of
    data. Lisp then prints the result of evaluating what we entered, which
    is the list itself.

    Placing a quote in fron of lists so that they won't be evaluated as a
    command is called "quoting". But using quoting, you can tell Lisp: "This
    next part isn't a command. It's just a chunk of data for my program."

    > '(expt 2 3)
    (expt (2 3)

    The quote character ' is short-hand for the function "quote". So these
    expression are equivalent:

    > (quote (+ 2 3))
    (+ 2 3)

    > '(+ 2 3)
    (+ 2 3)


    [4] LISTS IN LISP
    Lists are a crucial feature in Lisp. They are what hold all you Lisp
    code and data together. For example:

    > (expt 2 3)

    This piece of of code contains a symbol (expt) and two numbers, all tied
    together as a list, indicated by the parentheses.

    Lists in Lisp are held together by structures called "cons cells".     


    [5] CONS CELLS

|#
