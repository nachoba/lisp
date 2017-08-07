; Common Lisp: A Tutorial
; -----------------------
;
; Chapter 02 - The Structure of the language
; ------------------------------------------
; In Lisp you deal more often with individual forms and functions. A form is a single
; complete unit in a program. It can be one line, or many lines. Forms are also called
; expressions, or symbolic expressions.
; A function is an action to be performed on arguments.
;
; Defining a Function:
; --------------------
; The form is:
; (defun <name-of-the-function> (parameters)
;   (<body-of-the-function>))
;
; For example: A function that sum the squares of two numbers can be defined as:

(defun sum-of-square (a b)
  (+ (* a a) (* b b)))

; Operators:
; ----------
; The first item after the open parenthesis is an operator.
; Operators can be: functions, macros, or special forms (also called expressions).
;
; Functions: A function is an operator in the classic sense. It has a definition and
;            returns a value. Its arguments are evaluated before they are passed to
;            the function. A function is a separate entity that is applied to arguments.
;            The "defun" macro defines a new function.
;
; Macros   : Macros expand into a form (expression) that is translated into function calls
;            or special forms. The arguments are not evaluated before they are passed to the
;            macro. The macro returns a form that is handled by the evaluator before being
;            passed to the interpreter or compiler.
;            The "defmacro" defines a new macro.
;
; Special Forms : Special forms handle program control and bindings. These forms include:
;                 if, setq, quote, and progn. There is no way to define special forms.
;                 "quote" is a special form that returns its arguments unevaluated.
;                 For example: (+ 2 2)         => 4
;                              (quote (+ 2 2)) => (+ 2 2)
;                              '(+ 2 2)        => (+ 2 2)   ; The ' is shorthand for quote.
;
; There are many functions that take other functions as arguments. "quote" is used frequently
; so that the argument functions are not evaluated before they are given to other functions.
;
; Keywords that Aren't
; --------------------
; Functions are not keywords, neither are they reserved words. The names of functions are
; symbols. In all of Lisp, there are only a handful of functions that you cannot redefine.
; The symbol that names a function can be re-bound to any function definition. This is the
; nature of Lisp. You can define your own functions any time, you can also redefine just
; about anything in the language.
; A keyword in Lisp is a special symbol that defines how arguments are used in a function.
; Lambda keywords determine whether an argument is required, optional, to be gathered into
; a list, or its a keyword itself. When an argument is a keyword, the call to the function
; must include the keyword and its value.
;
; Everything is Subject to Change
; -------------------------------
; The power of Lisp comes from its flexibility. It has been referred to as a plastic language.
; You can stretch it, mold it, fold it, tear it down, and start over again.
;
; car, cdr, and cons
; ------------------
; While everything is subject to change, three functions are fairly constant among all Lisps.
; These are called primitive functions.
; car    : gives the first element of a list
; cdr    : pronounced could'er, gives the rest of the list
; cons   : build lists
;
; For example:

(car  '(a b c))      ; returns A
(cdr  '(a b c))      ; returns (B C)
(cons 'a '(b c))     ; returns (A B C)
(car (cdr '(a b c))  ; returns B

; Complex functions are built on other functions that use other functions, that use other
; functions, etc.
