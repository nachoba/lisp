#lang racket
#|
Simple Definitions and Expressions
----------------------------------

A program module is written as:

#lang <langname> <topform>

where a <topform> is either a <definition> or an <expr>.

Definitions
-----------
A definition of the form:

(define <id> <expr>)

binds <id> to the result of <expr>, while


(define (<id> <id> ...) <expr> ....)

binds the first <id> to a function (also called a procedure) that arguments as named by
the remaining <id>. In the function case <expr> ... are the body of the function. When
the function is called it returns the result of the last <expr>.
|#

(define pie 3)              ; define pie to be 3
(define (piece str)         ; defines piece as a function of one argument
  (substring str 0 pie))

#|
A function definition can include multiple expressions for the function's body. In that case,
only the value of the last expression is returned when the function is called.
|#

(define (bake flavor)
  (printf "pre-heating oven...\n")
  (string-append flavor " pie"))

#|
Racket programmers prefer to avoid side-effects, so a definition usually has just one expression
in its body. It's important, though, to understand that multiple expressions are allowed in a
definition body, because it explains why the following "nobake" function fails to include its
arguments in its result:
|#

(define (nobake flavor)
  string-append flavor "jello")

#|
Within "nobake", there are no parentheses around: string-append flavor "jello", so they are
three separate expressions instead of one function-call expressions. The expression string-append
and flavor are evaluated, but the results are never used. Instead, the result of the function
is just the result of the final expression, "jello".
|#


