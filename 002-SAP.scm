#|
+----------------------------------------------------------------------------------+
| CHAPTER 02 :: Procedures and Recursion                                           |
+----------------------------------------------------------------------------------+
|                                                                                  |
| [2.1] OVERVIEW                                                                   |
| One of the advantages of using Scheme  is that the number of procedures provided |
| by the language is relatively small, so we do not have to learn to use many pro- |
| cedures in order to write Scheme programs. Instead,  Scheme makes it easy for us |
| to define our own procedures as we need them. In this chapter, we discuss how to |
| define procedures to manipulate lists. We also discuss how  a procedure can call |
| itself within its definition, a process called recursion. Finally,  we introduce |
| an elementary tracing tool to help us in debugging programs.                     |
|                                                                                  |
| [2.2] PROCEDURES                                                                 |
| The notation f(x,y) is used in mathematics to denote a function; it has the name |
| f and has two variables, x and y.We call the values that are given to the varia- |
| bles the arguments of the function.  To each pair of arguments, the function as- |
| sociates a corresponding value.                                                  |
| In computing, we are concerned  with how that value is  produced, and  we  speak |
| about the sequence  of computational steps  that we perform to get the value re- |
| turned by the function as an algorithm for computing the function's value.       |
| The way we implement the  algorithm on the computer  to get the desired value is |
| called a procedure for computing the desired value.                              |
| If f is the name of the procedure with variables x and y, we use a list version, |
| (f x y), of the prefix notation f(x,y) used in mathematics.                      |
|                                                                                  |
| Scheme provides an elegant way of defining procedures based upon the lambda cal- |
| culus introduced by logician Alonzo Church. We illustrate this method with an e- |
| xample:                                                                          |
|                                                                                  |
|    When we write (cons 19 '()) , we get a list with one number in it, (19).  If  |
|    we write (cons 'bit '()), we get a list with one symbol on it, namely (bit).  |
|    Now let us write a procedure of one variable that  returns a list containing  |
|    the the value given to that variable as its only element.   We do it with an  |
|    expression called a "lambda":                                                 |
|                                                                                  |
|                                      (lambda (item) (cons item '()))             |
|                                                                                  |
|    A lambda expression is an  example of a sepcial form:  a form of  expression  |
|    identified by a special symbol called a keyword, in this case "lambda".   If  |
|    the procedure defined by this lambda expression is applied to 19,  the para-  |
|    meter "item", which is in the list following the keyword lambda, is assigned  |
|    (bound to) the value 19. Then the following subexpression (known as the body  |
|    of the lambda expression) is evaluated with the paramter item bound to 19.    |
|                                                                                  |
|    The  value of the body so obtained is  returned as the value of the applica-  |
|    tion. In this case, it returns the value of (cons item '()) , which is (19).  |
|    In summary, when a procedure that is the value of a lambda expression is ap-  |
|    plied to some value,  the parameter is bound  to that value, and the body of  |
|    the lambda expression is evaluated with this parameter binding. The value of  |
|    the body is returned as the value  of the application of the procedure.  The  |
|    lambda expression has the syntax                                              |
|                                                                                  |
|                                      (lambda (parameter ...) body)               |
|                                                                                  |
|    The keyword "lambda" is followed by a list that contains the parameters. The  |
|    three dots  following the paramter indicates  that the list contains zero or  |
|    more parameters.The next subexpression is the body of the lambda expression.  |
|    The value of a lambda  expression is the procedure, which can  be applied to  |
|    values appropriate for the evaluation of the body.These values must agree in  |
|    number with the  number of parameters  in the lambda  expression's parameter  |
|    list. When the procedure  is applied, the paramters are bound to the corres-  |
|    ponding values, and the body is evaluated. The value of the body is then the  |
|    value of the application.                                                     |
|                                                                                  |              
| In general, when a procedure is applied, the syntax is                           |
|                                                                                  |
|                                      (operator operand ...)                      |
|                                                                                  |
| where operator is a subexpression that evaluates to the procedure being applied, |
| and the operands are  subexpressoins that evaluate to the arguments to which the |
| procedure is applied.We stress that the arguments are the values of the operands |
| For example, in the application  (* (+ 2 3) (- 7 1)) the operator * evaluates to |
| the multiplication  procedure, the two operands are (+ 2 3) and (- 7 1), and the |
| two arguments are 5 and 6.  The value of the application is then 30, the product |
| of 5 and 6.                                                                      |
|                                                                                  |
| Thus to apply the procedure we defined above to build a list containing the sym- |
| bol "bit", we enter                                                              |
|                                                                                  |
|                                      ((lambda (item) (cons item '())) 'bit)      |
|                                   =>                                             |
|                                      (bit)                                       |
|                                                                                  |
|                                      ((lambda (item) (cons item '())) (* 5 6))   |
|                                   =>                                             |
|                                      (30)                                        |
|                                                                                  |
| It is awkward to write the whole expression (lambda (item) (cons item '())) each |
| time we want to apply the procedure. We can avoid this by giving the procedure a |
| name and using that name in the procedure  application. This is done by choosing |
| a name, say "make-list-of-one", for this  procedure and then defining it to have |
| the desired procedure as its value. We write                                     |
|                                                                                  |
|                                      (define make-list-of-one                    |
|                                        (lambda (item)                            |
|                                          (cons item '())))                       |
|                                                                                  |
| To apply the procedure "make-list-of-one", we enter the application              |
|                                                                                  |
|                                      (make-list-of-one 'bit)                     |
|                                   =>                                             |
|                                      (bit)                                       |
|                                                                                  |
| Computer programs to perform  various tasks are  written by defining  the appro- |
| riate procedure to accomplish the desired tasks.  Now let us  write a  procedure |
| called "make-list-of-two" that takes two arguments and returns a list whose ele- |
| ments are those two arguments:                                                   |
|#

(define make-list-of-one
  (lambda (item)
    (cons item '())))

(define make-list-of-two                                ; This procedure creates
  (lambda (item1 item2)                                 ; a list of two items
    (cons item1 (make-list-of-one item2))))


#|
| The parameter list following the keyword lambda consists of two parameters item1 |
| and item2. We apply the procedure "make-list-of-two" to the two symbols  one and |
| two by writing                                                                   |
|                                                                                  |
|                                      (make-list-of-two 'one 'two)                |
|                                   =>                                             |
|                                      (one two)                                   |
|                                                                                  |
| When we defined the procedure "make-list-of-two", we used the parameters "item1" |
| and "item2". When we applied the procedure "make-list-of-two", its two arguments |
| were the values of the operands  'one   and  'two.                               |
|                                                                                  |
| Now we will use the procedure "make-list-of-two" to define a procedure "regroup" |
| that has as its parameter list-of-4, which will be bound to a list of four items |
| It returns a list with the items in "list-of-4" regrouped  into two lists of two |
| items each. In the course of writing  the definition of "regroup", we shall find |
| it clearer to make use of certain  other procedures, which  express what we want |
| to appear in the list of the two items we create. We use these procedures in the |
| definition of "regroup" and then define them afterward.   The order in which the |
| definitions are written does not matter, and it's often more convenient to use a |
| procedure in the definition where it is needed, and then to define it later.  In |
| the definition that follows, we make use of two such "helping procedures",one is |
| called "first-group" and the other "second-group".                               |
|#

(define regroup
  (lambda (list-of-4)
    (make-list-of-two
     (first-group list-of-4)
     (second-group list-of-4))))

#|
| Procedure make-list-of-two is used to create a list of two items, the first item |
| being a list consisting of the first two items  in list-of-4 and the second con- |
| sisting of the last two items in list-of-4.  To construct the first grouping, we |
| use a helping procedure "first-group" that we define as:                         |
|#

(define first-group
  (lambda (ls)
    (make-list-of-two (car ls) (cadr ls))))

;; and we define "second-group" as:

(define second-group
  (lambda (ls)
    (cddr ls)))

(define menu '(chicken soup ice cream))

#|
| When "first-group" is applied to "list-of-4", the parameter "ls" is bound to the |
| list of four items  and the helping  procedure "make-list-of-two" is  applied to |
| build the  desired list  consisting of the  first two items in the list  of four |
| items.  Similarly, the helping procedure "second-group" produces the rest of the |
| list of four items following the first two, that  is, the list consisting of the |
| last two items. Now to get the new menu, we simply apply the procedure "regroup" |
| to "menu", and we get the desired list:                                          |
|                                                                                  |
|                                      (regroup menu)                              |
|                                   =>                                             |
|                                      ((chicken soup) (ice cream))                |
|                                                                                  |
| In general, we use helping procedures to make code easy to understand.           |
|                                                                                  |
| We have used and defined procedures to  build lists containing  one time and two |
| items. Scheme provides  a procedure "list",  which takes any number of arguments |
| and constructs a list containing those arguments. For example:                   |
|                                                                                  |
|                                      (list 'a 'b 'c 'd)                          |
|                                   =>                                             |
|                                      (a b c d)                                   |
|                                                                                  |
|                                                                                  |
|                                      (list '(1 2) '(3 4))                        |
|                                   =>                                             |
|                                      ((1 2) (3 4))                               |
|                                                                                  |
|                                                                                  |
|                                      (list)                                      |
|                                   =>                                             |
|                                      ()                                          |
|                                                                                  |
+----------------------------------------------------------------------------------+

  +------------------------------------------------------------------------------+
  | There are two styles of writing programs, top-down and bottom-up programming |
  | In both, we are looking for the solution of some problem and want to write a |
  | procedure that returns the desired solution as its value.  For now, we refer |
  | to this as the main procedure. In top-down style, we first write the defini- |
  | tion of the  main procedure.  The main procedure often  uses certain helping |
  | procedures, so we write the definitions of the helping procedures next.These |
  | in turn may require other helping procedures, so we write those,  and so on. |
  | In bottom-up style, we first write the definitions of the helping procedures |
  | that we anticipate using, and at the end, we write the main procedure.       |
  +------------------------------------------------------------------------------+

+----------------------------------------------------------------------------------+
| We summarize this discussion by  observing that the value of a lambda expression |
| with the syntax                                                                  |
|                                        (lambda (parameter ...) body)             |
|                                                                                  |
| is a procedure. When the procedure is applied, the  parameters are  bound to the |
| arguments (i.e., the values of the operands), and the body is evaluated.  We can |
| give the procedure a name by using a "define" expression with the structure:     |
|                                                                                  |
|                                        (define procedure-name lambda-expression) |
|                                                                                  |
| where the "procedure-name" is the variable used as the name of the procedure. We |
| apply (call or invoke) such a named procedure by writing the application         |
|                                                                                  |
|                                        (procedure-name operand ...)              |
|                                                                                  |
| where the number of operands matches the number of  paremeters in the definition |
| of the procedure. In general, when an application of the form                    |
|                                                                                  |
|                                        (operator operand ...)                    |
|                                                                                  |
| is evaluated,the operands and the operator are all evaluated in some unspecified |
| order. The operator must evaluate to a procedure. The values of the operands are |
| the arguments. The procedure binds the parameters to the arguments and evaluates |
| the body, the value of which is the value of the application.   Because the ope- |
| rands are first evaluated and it is their values, the arguments,  that the  pro- |
| cedure receives, we say the operands are passed by value to the procedure.       |
| We also encountered two expressions that are called "special" forms:  those with |
| the keywords "define" and "lambda".   These expressions are not applications be- |
| cause not all the items in the expressions are evaluated initially. For example, |
| in a lambda expression,the parameter list is never evaluated and its body is not |
| evaluated initially.                                                             | 
|                                                                                  |
| So, to define a procedure we have two methods in Scheme:                         |
|                                                                                  |
|                                   (define procedure-name lambda-expression)      |
|                                   (define item (lambda (item) (cons item '())))  |
|                                                                                  |
|                                   (define (procedure-name parameter ...) body)   |
|                                   (define (tim item) (cons item '()))            |
|                                                                                  |
+----------------------------------------------------------------------------------+

+----------------------------------------------------------------------------------+
| Exercises                                                                        |
+----------------------------------------------------------------------------------+
| [2.1] Define a procedure called "second" that takes as  its arguments a list and |
|       that returns the second item in the list. Assume that the list contains at |
|       least two items.                                                           |
|#

(define (second ls)
  (cadr ls))

#|
| [2.2] Define a procedure callled "third" that takes  as its arguments a list and |
|       that returns the third item in the list.  Assume that the list contains at |
|       least three items.                                                         |
|#

(define (third ls)
  (caddr ls))

#|
| [2.3] The procedure "first-of-both" is defined as follows:                       |
|                                                                                  |
|       (define first-of-both                                                      |
|         (lambda (list-1 list-2)                                                  |
|           (make-list-of-two (car list-1) (car list-2))))                         |
|                                                                                  |
|       Determine the value of the following expressions:                          |
|       (first-of-both '(1 3 5 7) '(2 4 6))           => (1 2)                     |
|       (first-of-both '((a b) (c d)) '((e f) (g h))) => ((a b) (e f))             | 
|#

(define first-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))

#|
| [2.4] Define a procedure "juggle" that rotates a three-element list.  The proce- |
|       dure returns a list that is a rearrangement of the input list so that  the |
|       first element of this list becomes the second, the second element  becomes |
|       the third, and the third element becomes the first. Test your procedure on |
|       (juggle '(jump quick spot))    => (spot jump quick)                        |
|       (juggle '(dog bites man))      => (man dog bites)                          |
|#

(define juggle
  (lambda (list-three)
    (add-to-list
      (car list-three)
      (cadr list-three)
      (caddr list-three))))

(define add-to-list
  (lambda (elem1 elem2 elem3)
    (list elem3 elem1 elem2)))

#|
| [2.5] Define a procedure "switch" that interchanges the first and third elements |
|       of a three-element list. Test your procedure on the examples given in  the |
|       previous exercise.                                                         |
|#
(define switch
  (lambda (list-three)
    (change-order
     (car list-three)
     (cadr list-three)
     (caddr list-three))))

(define change-order
  (lambda (elem1 elem2 elem3)
    (list elem3 elem2 elem1)))

#|
+----------------------------------------------------------------------------------+
| [2.3] CONDITIONAL EXPRESSIONS                                                    |
| Suppose we want to define a predicate that tests whether a value is a  number, a |
| symbol, an empty list, or a pair, and returns a symbol indicating its type.  The |
| structure of the test can be written in natural language as:                     |
|                                                                                  |
| * If the value is a pair, return the symbol "pair".                              |
| * If the value is an empty list, return the symbol "empty-list".                 |
| * If the value is a number, return the symbol "number".                          |
| * If the value is a symbol, return the symbol "symbol".                          |
| * Otherwise, return the symbol "some-other-type".                                |
|                                                                                  |
| The description of the procedure using English gives a sequence of steps that we |
| follow to carry out computation.Such a squence of steps describing a computation |
| is called an algorithm.   We implement the kind of "case analysis" given in this |
| algorithm using a "cond" expression (the special form with keyword "cond").  The |
| keyword "cond" is derived from the word conditional.    Using "cond", we write a |
| procedure called  "type-off" that tests its argument and returns the type of the |
| item as described above:                                                         |
|                                                                                  |                                  
|#

(define type-of
  (lambda (item)
    (cond
     ((pair? item) 'pair)
     ((null? item) 'empty-list)
     ((number? item) 'number)
     ((symbol? item) 'symbol)
     (else 'some-other-type))))

