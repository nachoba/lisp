#|
 
    filename:    lesson-005.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        05/09/2017
    revision:    
    description: Chapter 4 from "Land of Lisp"
                 We start to explore conditionals.

    contents:    [1] The Symmetry of "nil" and "()"
                 [2] The Four Disguises of ()

|#


#|

    [1] THE SYMMETRY OF nil AND ()
    Lisp commands and data structures are imbued with symmetry. Lisp philosophy
    strongly emphasizes the use of lists to store and manipulate information.
    Therefore, the design of Lisp favors behaviors that make it easy to slice
    and dice such lists.

    The most profound design decision made in Common Lisp, with regard to lists,
    is that it automatically treats an empty list as a false value when
    evaluating a condition:

|#

(defun i-am-false ()      ;; If we pass the empty list () into an "if" form
  (if '()                 ;; it evaluates as a false value.
      'i-am-true
      'i-am-false))

(defun i-am-true ()       ;; Whereas a list that contains at least one item
  (if '(1)                ;; evaluates to true.
      'i-am-true
      'i-am-false))

#|

    Because we can easily detect an empty list, we can process lists using
    recursion. With this technique, we can take items from the front of a list
    and send the rest of the list back to the same function until the list is
    empty.

    Following is a common recursive function that calculates the length of a
    list. This function calls itself recursively as it cuts items from the 
    front of the list.

|#

(defun my-length (lst)               ;; The function takes one argument; a list.
  (if lst                            ;; If the list is empty, return 0, else
      (1+ (my-length (cdr lst)))     ;; add 1 and call my-length again with the
      0))                            ;; rest of the list.




#|

    [2] THE FOUR DISGUISES OF ()
    Not only does the empty list evaluate to false, but is the only false value
    in Common Lisp: Any value not equivalent to an empty list will be considered
    a true value.

    However, there are some other expressions in Lisp that are disguises for the
    one and only empty list:
 
      +------+-----+
      | '()  | ()  |
      +------+-----+
      | 'nil | nil |
      +------+-----+
    We can see that the expression in this table are equivalent by comparing
    them with one another using the "eq" command:

    > (eq '() nil )
    > (eq '() ()  )
    > (eq '() 'nil)

    All evaluate to true: T 

    Notice that the only value in the table that seems "normal" is the quoted
    list on the left side of the comparisions. The other tree, on the right,
    seem to break the rules of Lisp forms that we talked about in previous
    lessons.

    Common Lisp is architected to make sure all four of these values look like
    an empty list when you use them in your programs, allowing most Lisp
    conditionals to be written with an elegant brevity.

    (First Case) (eq '() nil)
    There's a constant named "nil" that evaluates to itself and allows you to
    omit the quotation mark.

    (Second Case) (eq '() ())
    This is the natural way Common Lisp parses and empty list.

    (Third Case) (eq '() 'nil)
    This is due to a requirement in the Common Lisp spec that says that () and
    "nil" should be treated the same, therefore '() and 'nil should also be.
    
|#


