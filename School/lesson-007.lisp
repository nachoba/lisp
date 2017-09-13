#|
 
    filename:    lesson-007.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        06/09/2017
    revision:    
    description: Chapter 4 from "Land of Lisp"
                 Continue exploring conditionals.

    contents:    [1] Cool Tricks with Conditions
                 [2] Using the "and" and "or" Conditionals
                 [3] Using Functions that Return More than Just the Truth
                 [4] Comparing Stuff: "eq", "equal", and More
                 [5] Summary
                

|#


#|

    [1] COOL TRICKS WITH CONDITIONS
    A couple of counterintuitive tircks involving conditions in Lisp can help
    you write cleaner code. 


    [2] USING THE "and" AND "or" CONDITIONALS
    The conditionals "and" and "or" are simple mathematical operators, which
    allow you to manipulate Boolean values.

    "and" returns true "t" if all conditions are satisfied.
    "or"  returns true "t" if at least one condition is satisfied.

    > (and (oddp 5) (oddp 7) (oddp 9))
    T

    > (or (oddp 4) (oddp 7) (oddp 8))
    T


    But there's something more interesting about these operators. These commands
    look like ordinary mathematical operators; they do not look like conditional
    operators such as "if" or "cond". However, they can be used for conditional
    behavior.

    Here's how we could use these conditionals to set a global variable to true
    only when a number is even:

|#

(defparameter *is-it-even* nil)

(or (oddp 4) (setf *is-it-even* t))

(princ *is-it-even*)


#|

    This returns true "T". If we do the same using an odd number, the variable
    remains unchanged.

    This example illustrates that Lisp uses "shortcut Boolean evaluation". This
    means that once Lisp determines that an earlier statement in a list of "or"
    values is true, it simply returns true and doesn't bother evaluating the
    remaining statements.

    Similarly, once it determines that an earlier statement in a list of "and"
    values is false, it stops without bothering to evaluate the rest of the
    statements.    

    This is not a minor observation as it can actually be very useful in many
    situations. For instance, imagine if you want to save a file to disk, but
    only if the file was modified, and only when the user wants it to be saved.
    The basic structure could be as follows:

    > (if *file-modified*
        (if (ask-user-about-saving)
            (save-file)))


    However, since shortcut Boolean evaluation is guaranteed to be used for
    Boolean operations under Common Lisp, we could write this instead:

    > (and *file-modified* (ask-user-about-saving) (save-file))

    Using this style for evaluating conditional code is possible only if you
    think beyond the typical use of the Boolean operators as simply mathematical
    operators.

    There a caveat, though, a reader of your code may easily miss the fact that
    (save-file) does something beyond returning a Boolean value.

    A third way of writing this code, which is a compromise between the previous
    approaches is:

    > (if (and *file-modified*
               (ask-user-about-saving))
          (save-file))

    Most experienced lispers will consider this version a bit clearer because
    only expressions that are expressly designed to return a Boolean value are
    treated as part of the condition.

 
 
    [3] USING FUNCTIONS THAT RETURN MORE THAN JUST THE TRUTH
    Any value in Common Lisp (except for the different variations on "nil") is
    true. This means that functions that are commonly used in conditions have
    the option of returning more than just the truth.

    For instance, the command "member" can be used to check for list membership
    for an item:

    > (if (member 1 '(3 4 1 5))
          'one-is-in-the-list
          'one-is-not-in-the-list)

    ONE-IS-IN-THE-LIST

    This is pretty straightforward, but there is something behind this. Let's
    look at the expression (member 1'(3 4 1 5) in isolation:

    > (member 1 '(3 4 1 5))

    (1 5)

    Why is returning (1 5)? When you write a function that returns true and false
    it is good practice to think if there is anything else that it could be
    returned instead of just "T".

    Sine any non-nil values in Common Lisp evaluate to true, returning some other
    value is essential a frebbie.

    The implementers of the "member" function decided that it would be valuable
    to return the value plus the tail of the list.

    But why doesn't it just return the value if found, instead of the tail? This
    would have been a useful way to define the member function as it would allow
    passing the original value to some other function. Unfortunately, one edge
    case in particular would ruin this plain.

    > (if (member nil '(3 4 nil 5))
          'nil-is-in-the-list
          'nil-is-not-in-the-list)

    NIL-IS-IN-THE-LIST

    The "member" function still gives the correct answer, even when we search for 
    "nil" as the member. If the "member" function had actually returned "nil" (the
    original value we were searching for), it would have evaluated as false, and the
    example would have incorrectly stated that "nil" isn't in the list.


    One function that really benefits from rich return values is "find-if":
  
    > (find-if #'oddp '(2 4 5 6))
    
    5


    > (if (find-if #'oddp '(2 4 5 6))
          'there-is-an-odd-number
          'there-is-no-odd-number)

    THERE-IS-AN-ODD-NUMBER

    The "find-if" function takes another function (in this case "oddp") as a parameter.
    "find-if" will find the first value in the list for which "oddp" returns true. You
    can see clearly how "find-if" can fill dual roles; either as a retriever of values
    matching some constraint or as a true/false value inside a condition.

    But if we try our edge case again, searching for a "nil" value, we get a strange
    result:

    > (find-if #'null '(2 4 nil 6))

    NIL

    The "null" function, which returns true for any of the "nil" values, correctly find
    the "nil". Unfortunately, in this one annoying case, we would not want to use the
    "find-if" inside a conditional statement, because a correctly found value still
    returns a result that evaluates as false.


    [4] COMPARING STUFF: eq, equal, AND MORE
    If you want to compare two values in Lisp to find out if they are "the same" you
    will find a hughe asortment of different functions that purport to accomplish this.
    Of these: "equal", "eql", "eq", "=", "string-equal", and "equalp" are the most
    commonly used.

    A lisper must understand the subtleties of these functions intimately in order to
    know how to compare values correctly.

    The simplest rule to have in mind is: 
    1. Use "eq" to compare symbols.
    2. Use "equal" for everything else.


    The "eq" function is the simplest of all the Lisp comparision functions, and it's
    also very fast. It doesn't really work for comparing items besides symbols, but as
    symbols play a central role in Lisp, this is a very useful function.
|#
