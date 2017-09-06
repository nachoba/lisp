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

|#
