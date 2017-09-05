#|
 
    filename:    lesson-004.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        05/09/2017
    revision:    
    description: Chapter 3 of "Land of Lisp"
                 In this lesson we will take a look at basic list functions.

    contents:    [1] Introduction
                 [2] The cons Function
                 [3] The car and cdr Functions
                 [4] The list Function
                 [5] Nested Lists
                 [6] Summary

|#


#|

    [1] INTRODUCTION
    Manipulating lists is extremely important in Lisp programming. There
    are three basic functions for manipulating cons cells (and hence lists)
    in Lisp: cons, car, and cdr.

    [2] THE cons FUNCTION
    If you want to link any two pieces of data in your Lisp program (no
    matter the type), the usual way to do that is with the "cons" function.
    When you call "cons", the Lisp compiler typically allocates a small
    chunk of memory, the cons cell, that can hold two references to the
    objects being linked.

    > (cons 'chicken 'cat)
    (CHICKEN . CAT)

    The "cons" function, returns a single object, the cons cell, represented
    by parentheses and a dot between the two connected items. Don't confuse
    this with a regular list. The dot in the middle makes this a cons cell,
    just linking those two items together.

    If instead of another piece of data, we attach the symbol "nil" on the
    right side of the list:

    > (cons 'chicken 'nil)
    (CHICKEN)

    As you see, "nil" does not show in the output this time. "nil" is a
    special symbol that is used to terminate a list in Lisp.
    Note that the Lisp interpreter could have displayed the result by
    explicitly showing our cons cell and printing (CHICKEN . NIL), but
    Lisp will always go out of its way to "hide" the cons cells from you.
    When it can, it will show your results using lists. 
    It will show a cons cell (with the dot between the objects) only if
    there isn't a way to show your results using lists.

    These expressions are equivalent:

    > (cons 'chicken 'nil)
    (CHICKEN)

    > (cons 'chicken ())
    (CHICKEN)

    That is because the empty list, (), can be used interchangeably with
    the "nil" symbol in Common Lisp.

    The "cons" function also can add a new item to the front of the list.

    > (cons 'pork '(beef chicken))
    (PORK BEEF CHICKEN)

    When lispers talk about using "cons", they say they are "consing"
    something.


    Since all lists are made of cons cells, we can decompose the previous
    list as:

    > (cons 'pork (cons 'beef (cons 'chicken ())))
    (PORK BEEF CHICKEN)

    So in Lisp, a chain of cons cells and a list are exactly the same thing.


    [3] THE car AND cdr FUNCTIONS
    Lists are just long chains of two-item cells.

    The "car" function is used for getting the thing out of the first slot,
    or the remainder of a list:

    > (car '(pork beef chicken))
    PORK

    The "cdr" function is used to grab the value out of the second slot, or
    the remainder of a list:

    > (cdr '(pork beef chicken))
    (BEEF CHICKEN)

    You can string together "car" and "cdr" into new functions like "cadr",
    "cdar", "cadadr", etc. This lets you succinctly extract specific pieces
    of data out of complex lists.

    > (cadr '(pork beef chicken))
    BEEF

    > (car (cdr '(pork beef chicken)))
    BEEF


    [4] THE list FUNCTION
    For convenience, Common Lisp has many functions built on top of the
    basic three -"cons", "car", and "cdr". A useful one is the "list"
    function, which does the dirty work of creating all the cons cells and
    builds our list all at once:

    > (cons 'pork (cons 'beef (cons 'chicken ())))
    can be expressed as:
    > (list 'pork 'beef 'chicken)
    (PORK BEEF CHICKEN)


    [5] NESTED LISTS
    Lists can contain other lists. Following is a list containing three
    items. The second item of this list is (duck bat), which is a list
    itself.

    > '(cat (duck bat) ant)

    Under the hood, these nested lists are still just made out of cons
    cells. So, cons cells allow us to create complex structures, including
    nested lists.

    Common Lisp already defines all these functions for you. You can use
    any function with the name c*r right out of the box, up to four levels
    deep. In other words, "cadadr" will already exist for you to use,
    whereas "cadadar" (which is five levels deep) does not and you will have
    to write that function yourself.

    These functions make it easy to manipulate cons cells-based structures
    in Lisp, no matter how complicated they might be.

    Exercise: Create the following list using the "cons" function only.
    ((peas carrots tomatoes) (pork beef chicken))
|#
(defparameter *exercise*
   (cons (cons 'peas (cons 'carrots (cons 'tomatoes ())))
         (cons (cons 'pork (cons 'beef (cons 'chicken ()))) ())))

#|

    [6] SUMMARY
    
    * Parentheses in Lisp are there to keep the amount of syntax to the
      minimum.
    * Lists are created from cons cells.
    * You can create lists by making cons cells with the "cons" command.
    * You can inspect the pieces of a list with "car" and "cdr".

|#


	
