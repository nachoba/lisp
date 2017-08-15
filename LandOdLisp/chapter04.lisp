#|
CHAPTER 04 :: Making Decisions with Conditions
----------
In this chapter, we'll be looking in detail at commands for handling conditions.
The elegance of these commands shows that the unusual philosophy and design of
Lisp has real practical benefits.

The Symmetry of "nil" and "()"
------------------------------
One thing is particularly striking when we look at how Lisp commands and data
structures work: They are imbued with symmetry in every conceivable way. This
symmetry can give your Lisp code a certain elegance that other languages cannot
have, and Lisp's simple syntax is an important factor in making this symmetry
possible.

Empty Equals False
------------------
Since the Lisp philosophy strongly emphasizes the use of lists to store and
manipulate information, it will come as no surprise that the design of CL,
flavors behaviors that make it easy to slice and dice such lists.
The most profound design decision made in Common Lisp, with regard to lists,
is that it automatically treats an empty list as a false value when evaluating
a condition:

> (if '()
      'i-am-true
      'i-am-false)

I-AM-FALSE

> (if '(1)
      'i-am-true
      'i-am-false)

I-AM-TRUE

Because we can easily detect and empty list, we can process lists using recursion.
With this technique, we can take items from the front of a list and send the rest
of the list back to the same function until the list is empty.

Let's look at a common recursive function which calculates the length of a list:
|#

(defun my-length (list)
  (if list
      (+ 1 (my-length (cdr list)))
      0))

#|
The Four Disguses of ()
-----------------------
Not only does the empty list evaluate to false, but it is the only false value in 
Common Lisp. Any value not equivalent to an empty list will be considered a true
value. All the following expressions are equivalente and are different disguises for
the one and only empty list:

() = '() = nil = 'nil

The Conditionals: if and Beyond
-------------------------------
Now that we understand how Lisp handles true and false, let's look at if and some
of the other conditional commands.

One Thing at a Time with if
---------------------------
The "if" command can be used to make different things happen when things are true
or false.

> (if (= (+ 1 2) 3)
      'yup
      'nope)

YUP

> (if (= (+ 1 2) 4)
      'yup
      'nope)

NOPE
s

