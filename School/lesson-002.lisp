#|
 
    filename:    lesson-002.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        05/09/2017
    revision:    
    description: Chapter 3 of "Land of Lisp".
                 This chapter explores the syntax of Lisp.

    contents:    [1] Syntax and Semantics
                 [2] The Building Blocks of Lisp Syntax
                 [3] Symbols
                 [4] Numbers
                 [5] Strings

|#



#|

    [1] SYNTAX AND SEMANTICS
    Lisp commands must be entered in a rather unorthodox way, with 
    parentheses surrounding each command. In this chapter, we'll explore 
    why Lisp works this way.

    To understand why any language looks a certain way, we need to begin
    with two concepts from the field of linguistics: syntax and semantics.

    The syntax of a piece of text represents the basic rules that it needs
    to follow to be a valid sentece. When we talk about the semantics of
    a sentence, we're referring to its meaning.

    The actions that a program performs are the semantics of the program.
    It is usually possible to write a program that has the same semantics
    in different programming languages; that is, the program will do the
    same thing in both languages. Most programming languages have similar
    semantic powers.

    So programming languages differ in their syntax. Having a simple syntax
    is a defining feature of the Lisp language.


    [2] THE BUILDING BLOCKS OF LISP SYNTAX
    Some programming languages (C++ for instance) have a weird syntax. That
    translates into a lot of work to write a compiler for that language.
    Writing a Lisp compiler or interpreter is much easier. The part of a
    Lisp compiler or interpreter that reads in the code -which Lispers
    actually call "the reader"- is simpler than that of most programming
    languages.

    Lisp has only one way of organizing bits of code: It uses parentheses
    to organize the code into lists. All basic Lisp code uses this simple
    list-like syntax:

    (bla bla bla bla bla)

    What sorts of things can we put into these lists?
    Other lists, symbols, numbers, and strings, for instance.
    We will look at these basic building blocks, or datatypes.


    [3] SYMBOLS
    Symbols are a fundamental type of data in Lisp and are used extensively.
    A symbol in Lisp is a stand-alone word. Symbols in Common Lisp are case-
    insensitive, although most Lispers avoid using uppercase.


    [4] NUMBERS
    Lisp supports both floating-point numbers and integers. When you write
    a number, the presence of a decimal point determines whether your
    number is seen as a floating-point number or an integer.
   
    If you use most math functions with both an integer and a floating-point
    number, the integer will become "poisoned", and a floating-point number
    will be returned:

    > (+ 1 1.0)
    2.0

    You should know that something weird could happen if you divide two
    integers:

    > (/ 4 6)
    2/3

    Note that we get a different answer if there is a floating-point number
    in our calculation:

    > (/ 4 6.0)
    0.66666667


    [5] STRINGS
    Strings aren't really that fundamental to Lisp from a theoretical
    standpoint; but any program that communicates with a human will usually
    need strings.

    To indicate a string in Lisp, surround characters with double quotes.
    We can display a string using a function called "princ"

    > (princ "Tutti Frutti")
    Tutti Frutti
    "Tutti Frutti"

    Notice that printing our text at the REPL will cause the text to appear
    twice. First, we see the actual printing caused by the "princ" command.
    However, since the REPL will always show the result of evaluating the
    entered expression, we see our string parroted back to us.

    A string can also contain so-called "escaped characters":

    > (princ "He yelled \"Stop that thief!\" from the busy street.")
    He yelled "Stop that thief!" from the busy street.
    "He yelled \"Stop that thief!\" from the busy street."
    

|#

