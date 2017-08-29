#|
Iterations in Lisp
------------------
When we want to do something repeatedly, it is sometimes more natural to use iteration
than recursion. A typical case for iteration is to generate some sort of table.
|#
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~a ~a~%" i (* i i))))

#|
The "do" macro is the fundamental iteration operator in CL. Like "let", "do" can create
variables, and the first argument is a list of variable specifications. Each element of
this list can be of the form:

                    (variable initial update)

where "variable" is a symbol, and "initial" and "update" are sub-expressions. Initially
each "variable" will be set to the value of the corresponding "initial"; on each iteration
it will be set to the value of the corresponding "update".
The "do" in "show-squares" creates just one variable, "i". On the first iteration "i" will
be set to the value of "start", and on successive iterations its value will be incremented
by one.
The second argument to "do" should be a list containing one or more expressions. The first
expression is used to test whether iteration should stop. In the case above, the test
expression is "(> i end)".
The remaining expressions in this list will be evaluated in order when iteration stops, and
the value of the last will be returned as the value of the "do". So "show-squares" will
always return "done".
The remaining arguments to "do" comprise the body of the loop. They will be evaluated, in
order, on each iteration. On each iteration the variables are updated, then the termination
test is evaluated, and then (if the test failed) the body is evaluated.
For comparison, here is a recursive version of "show-squares":
|#

(defun show-squares-recursive (i end)
  (if (> i end)
      'done
      (progn
        (format t "~a ~a~%" i (* i i))
        (show-squares-recursive (+ i 1) end))))

#|
The only thing new in this function is "progn". It takes any number of expressions,
evaluates them in order, and returns the value of the last.

To iterate through the elements of a list, for example, you would be more likely to
use "dolist".
|#

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length-recursive (lst)
  (if (null lst)
      0
      (+ (our-length-recursive (cdr lst)) 1)))
