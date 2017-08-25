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
|    the value given to that variable as its only element.We do it with an expres- |
|    sion called a "lambda":                                                       |
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
| We have used and defined procedures to  build lists containing  one item and two |
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

#|
| Let us analize the "cond" expression. In this case, the cond expression has five |
| clauses, each represented by  two expression enclosed  in parentheses. The first |
| clause, ((pair? item) 'pair), has as its first expression (pair? item), which is |
| a boolean expression with the value #t or #f depending on whether the value that |
| it is bound to item  is or is not a pair. We shall also refer to this expression |
| as the condition. If the condition evaluates to true, then the second expression |
| in the clause (the consequence), 'pair, is evaluated and pair is returned.If the |
| condition in the first  caluse evaluates to false,  the condition in  the second |
| clause ((null? item) 'empty-list) is evaluated.  If one of the subsequent condi- |
| tions is true, then its consequent is evaluated and that value is returned.  The |
| last clause has the keyword "else" as it first expression,and if all of the pre- |
| creding conditions are false,the expression following else is evalauted, and its |
| value is returned.  The expression following else is referred to as the alterna- |
| tive. In general, the syntax of a "cond" expression is                           |
|                                                                                  |
|                       (cond                                                      |
|                         (condition1 consequent1)                                 |
|                         (condition2 consequent2)                                 |
|                               .          .                                       |
|                         (conditionN consequentN)                                 |
|                         (else alternative))                                      |
|                                                                                  |
| where for each k = 1,...,N,  the expression (conditionk consequentk) and the ex- |
| pression (else alternative) are called clauses.  The conditionk and consequentk, |
| for k = 1,...,N, and the alternative are expressions, and else is a keyword.Each |
| of the conditional parts of the clauses is  evaluated in succession until one is |
| true, in which case the corresponding  consequent is evaluated, and the value of |
| the cond expression is  the same as the value of the consequent corresponding to |
| the true condition.   If none of the conditions is true, the cond expression has |
| the same value as the alternative, which is in the last cond clause, know as the |
| else clause.                                                                     |
| Note that the else clause is optional.If it is omitted and all of the conditions |
| are false,  then Scheme does not specify the value that is returned as the value |
| of the cond expression. We shall avoid using cond expressions that return unspe- |
| cified values.                                                                   |
| Scheme has another way of handling  conditional expressions  that have only  two |
| cases. We can also use the special form with keyword if.Suppose we want to write |
| a procedure "car-if-pair" that does the following:                               |
|                                                                                  |
|        If its argument is a pair, return the car of the pair.                    |
|        Otherwise, return the argument.                                           |
|                                                                                  |
| Here is the procedure "car-if-pair" using cond:                                  |
|#

(define car-if-pair
  (lambda (item)
    (cond
     ((pair? item) (car item))
     (else item))))

;; Or using an if expression, it can be written as:

(define car-if-pair2
  (lambda (item)
    (if (pair? item)
	(car item)
	(item))))

#|
| In general, the syntax of an if expression is:                                   |
|                                                                                  |
|          (if condition consequent alternative)                                   |
| or                                                                               |
|          (if condition consequent)                                               |
|                                                                                  |
|                                                                                  |
| In the first case, if condition  is true, the value of consequent is returned as |
| the value of the if expression; if condition is  false, the value of alternative |
| is returned as the value of the if expression.  In the second case, the alterna- |
| tive is not present.  In this "one-armed if", if condition is true, the value of |
| consequent is returned as the value of the if expression. If it is false, an un- |
| specified value is returned.                                                     |
| If expressions can be nested, enabling us to write the procedure "type-of" given |
| above as follows:                                                                |
|#

(define type-of2
  (lambda (item)
    (if (pair? item)
	'pair
	(if (null? item)
	    'empty-list
	    (if (number? item)
		'number
		(if (symbol? item)
		    'symbol
		    'some-other-type))))))

#|
| Any cond expression can be written as nested if expressions,but as the number of |
| cases increases, the nesting of the if expressions gets deeper,  and the meaning |
| of the whole conditional expression is obscured.This, using a cond expression is |
| often advantageous when there are several cases.                                 |
| The use of conditional expressions with either if or cond  depends upon first e- |
| valuating a condition.The condition may be simple, such as (null? ls), or it may |
| involve something like testing  whether ls is a pair and whether its car is some |
| symbol such as cat.A condition that involves a combination of two or more simple |
| conditions is called a compound condition.We build compound conditions by combi- |
| ning simple conditions with the logical composition operators: and, or, and not. |
| The compound condition mentioned above can be written using "and" as follows:    |
|                                                                                  |
|                   (and (pair? ls) (eq? (car ls)) 'cat)                           |
|                                                                                  |
| The syntax for each of these logical operators is given below:                   |
|                                                                                  |
|                   (and expr1 expr2 ... exprn)                                    |
|                   (or  expr1 expr2 ... exprn)                                    |
|                   (not expr)                                                     |
|                                                                                  |
| The "and" expression evaluates each of the subexpressions: expr1,expr2,...,exprn |
| in succession.   If any one of them is true, it stops evaluating the rest of the |
| subexpressions, and the value of the "and" expression is #f.  If all of the sub- |
| expressions have true values, the value of the last subexpression is returned as |
| the value of the "and" expression.                                               |
|                                                                                  |
| The "or" expression evaluates each of the subexpressions:  expr1,expr2,...,exprn |
| in succession.   If any one of them is true, it stops evaluation the rest of the |
| subexpressions,  and the value of the "or" expression is the value of that first |
| true subexpression.If all of the subexpressions are false, the value of the "or" |
| expression is #f.                                                                |
|                                                                                  |
| The value of the "not" expression is #f when expr has a true value, and it is #t |
| when expr is false.                                                              |
| We illustrate the use of "and" and "or" in the following example:                |
|#

(define s-and-n-list?
  (lambda (ls)
    (and (pair? ls)
	 (symbol? (car ls))
	 (pair? (cdr ls))
	 (number? (cadr ls)))))

#|
| The predicate "s-and-n-list?" takes a list as its argument. The value of the ex- |
| pression (s-and-n-list? some-list) is #t if:                                     |
|                                                                                  |
| * some-list is a pair,                                                           |
| * and the first item in some-list is a symbol,                                   |
| * and the cdr of some-list is a pair,                                            |
| * and the second item in some-list is a number.                                  |
|                                                                                  |
| Otherwise, the value of (s-and-n-list? some-list) is #f. For example:            |
|                                                                                  |
|                   (s-and-n-list? '(a 1 b))  => #t                                |
|                   (s-and-n-list? '(a b 1))  => #f                                |
|                                                                                  |
| The test to determine  whether the list is a pair is necessary since we can only |
| take the car of a pair.   If the list is empty, the evaluation of the car of the |
| list never takes place. The evaluation terminates on the first false value.      |
|#

(define s-or-n-list?
  (lambda (ls)
    (and (pair? ls)
	 (or (symbol? (car ls))
	     (number? (car ls))))))

#|
| The predicate "s-or-n-list?" takes a list as its argument.  The expression is #t |
| if:                                                                              |
|                                                                                  |
| * some-list is a pair,                                                           |
| * and either the first item in some-list is a symbol or a number.                |
|                                                                                  |
| Otherwise (s-or-n-list? some-list) is #f.                                        |
| There are occasions  when we want to  test whether a list contains precisely one |
| item, that is, whether the list is a singleton list. It is easy to define a pre- |
| dicate  "singleton-list?"  that tests whether its argument is a pair and whether |
| it contains just one element.  To test whether a pair contains just one element, |
| it is enough to test whether its cdr is empty. Thus we can write:                |
|#

(define singleton-list?
  (lambda (ls)
    (and (pair? ls) (null? (cdr ls)))))

#|
| This definition makes use of the fact that the empty list is not a pair.Thus the |
| nonempty  list whose cdr is empty must contain just one  item and is thus a sin- |
| gleton list.                                                                     |
|                                                                                  |
| Exercises                                                                        |
+----------------------------------------------------------------------------------+
| [2.6] Assume that a, b, and c are expressions that evaluate to #t and that e and |
|       f are expressions that evaluate to #f.    Decide whether the following ex- |
|       pressions are true or false.                                               |
|      a. (and a (or b e))              => (and #t (or #t #f))               => #t |
|      b. (or e (and (not f) a c))      => (or #f (and #t #t #t))            => #t |
|      c. (not (or (not a) (not b)))    => (not (or #f #f))                  => #t |
|      d. (and (or a f) (not (or b e))) => (and (or #t #f) (not (or #t #f))) => #f |
|                                                                                  |
| [2.7] Decide whether the following expressions are true or false if expr is some |
|       boolean expression.                                                        |
|      a. (or (symbol? expr) (not (symbol? expr)))                           => #t |
|      b. (and (null? expr) (not (null? expr)))                              => #f |
|      c. (not (and (or expr #f) (not expr)))                                => #t |
|      d. (not (or expr #t))                                                 => #f |
|                                                                                  |
| [2.8] Decide whether the following expressions  are true or false using the pro- |
|       cedure "s-and-n-list?" as defined in this section.                         |   
|      a. (s-and-n-list? '(2 pair 12 dozen))        => #f                          |
|      b. (s-and-n-list? '(b 4 u c a j))            => #t                          |
|      c. (s-and-n-list? '(a ten))                  => #f                          |
|      d. (s-and-n-list? '(a))                      => #f                          |
|                                                                                  |
| [2.9] Decide  whether the following expressions are true or false using the pro- |
|       cedure "s-or-n-list?" as defined in this section.                          |
|      a. (s-or-n-list? '(b))                       => #t                          |
|      b. (s-or-n-list? '(c 2 m))                   => #t                          |
|      c. (s-or-n-list? '(10 10 10 10))             => #t                          |
|      d. (s-or-n-list? '())                        => #f                          |
|                                                                                  |
+----------------------------------------------------------------------------------+
| [2.4] RECURSION                                                                  |
| We saw up that certain procedures use other procedures as helping procedures. In |
| this section, we  define procedures  that use themselves as helping  procedures. |
| When a procedure  calls itself within the body of the lambda expression defining |
| it, we say that the procedure is recursive. To introduce the idea of a recursive |
| procedure, we set as our goal the definition of a procedure last-item, that when |
| applied to a nonempty list, returns the last top-level item in the list.Here are |
| some examples of applications of last-item:                                      |
|                                                                                  |
|                   (last-item '(1 2 3 4 5))                 => 5                  |
|                   (last-item '(a b (c d)))                 => (c d)              |
|                   (last-item '(cat))                       => cat                |
|                   (last-item '((cat)))                     => (cat)              |
|                                                                                  |
| It is a good idea to begin with the simplest cases of the arguments to which the |
| procedure is applied. In this case, the simplest nonempty list is a list contai- |
| ning only one item.  For example, if the list is (a), then the last item is also |
| the first item, and applying car to this list produces the last item. This would |
| work with any list containing  only one top-level item, for the  car of the list |
| is both its first and its last top-level item. Let us use the variable ls as the |
| parameter in the definiton of last-item.How can we test whether ls contains only |
| one top-level item? When ls contains only one top-level item,its cdr is the emp- |
| ty list. Thus the boolean expression (null? (cdr ls)) returns #t when  -and only |
| when-  the nonempty list ls contains only one top-level item. Thus, we may use a |
| cond expression to test  whether we have the case of  a one-item list and return |
| the car of the list if that is the case.   We can then begin our program as fol- |
| lows:                                                                            |
|                                                                                  |
|                   (define last-item                                              |
|                     (lambda (ls)                                                 |
|                       (cond                                                      |
|                         ((nul? (cdr ls)) (car ls))                               |
|                         ... )))                                                  |
|                                                                                  |
| If we consider now a list ls containing more than one top-level item, the cdr of |
| that list contains one fewer top-level items,but still includes the last item of |
| the original list. Each successive application of cdr reduces the number of top- |
| level items by one, until we finally  have a list containing only one  top-level |
| item, for which we have a solution.                                              |
| In this sense,  application of cdr to the list reduces  the problem to a simpler |
| case. This leads us to consider the list obtained by  evaluating (cdr ls), which |
| contains all of the items of ls except its first item. The last item in (cdr ls) |
| is the same as the last item in ls.   For example, the list (a b c) and the list |
| (b c), which is its cdr, have the same last item, c.  Thus if we call the proce- |
| dure last-item as a helping  procedure to be applied to (cdr ls), we get the de- |
| sired last item of the original list, and that solves our problem.  Thus to com- |
| plete the  definition of  last-item, we add  the else clause  to handle the case |
| the list contains more than one item:                                            |
|#

(define last-item
  (lambda (ls)
    (cond
     ((null? (cdr ls)) (car ls))
     (else (last-item (cdr ls))))))
#|
| To see that this does define the procedure last-item so that it returns the cor- |
| rect result for any non empty list "ls", we consider first a list (a) containing |
| only one item.  Then te condition in the first cond clause is true, and (car ls) |
| does give us the last (which is also the first) item, a, in the list.  Thus, the |
| procedure last-item works on any list containing only one item. Now let's consi- |
| der the case in which ls is a list (a b) containing two items.Then its cdr, (b), |
| contains one item, so the procedure last-item does work for (cdr ls),allowing us |
| to use it as a helping procedure  in the else clause  to get the correct result. |
| Thus last-item solves the problem for any list of two items. Now we use the fact |
| that last-item works on the cdr of any three-item list to conclude that it works |
| on the three-item list itself. We can continue this process of increasing by one |
| the number of items in the list indefinitely, showing  that last-item solves the |
| problem for any list.                                                            |
| Since the procedure last-item called itself as a helping procedure, last-item is |
| a recursive procedure. Our strategy in general in designing a recursive procedu- |
| re on a list is first to identify the  "simplest case"  and write the expression |
| that solves the problem for that case as the consequent in the first cond clause.|
| We call this simplest case the "base case" or "terminating condition".   We then |
| identify a simplifying operation, which on repeated application to the list pro- |
| duces the base case.  Then in each of the other cases, we solve the problem with |
| some expression  that calls the  recursive procedure as a helping  procedure ap- |
| plied to the simplified list.  In our example, the base case is the list consis- |
| ting of only one item. The simplifying operation is cdr, and in the other cases, |
| we see that the expression that solves the problem applies last-item to the sim- |
| plified list (cdr ls).                                                           |
|                                                                                  |
| To give us a better intuition about how last-item works, we shall apply the pro- |
| cedure last-item to the list (a b c):                                            |
| 1. (last-item '(a b c)) => (cdr ls) is not empty, so the alternative in the else |
|    clause is evaluated. So we have to evaluate (last-item (cdr ls))              |
| 2. (last-item (cdr ls)) => Since (cdr ls) is (b c),  we must evaluate the follo- |
|    wing expression (last-item '(b c))                                            |
| 3. (last-item '(b c))   => Once again, (cdr ls) is not empty, so we evaluate the |
|    alternative in the else clause. This tells us to apply last-item to (cdr ls), |
|    which is now (c).                                                             |
| 4. (last-item '(c))     => This time (cdr ls) is the empty list. Thus the conse- |
|    quent is evaluated, that is, (car '(c)) evaluates to c.                       |
|                                                                                  |
| The recursion in the illustration  stops when the list is simplified to the base |
| case. In that case, the condition in the first cond clause is true.  We call the |
| condition used to stop the recursion the "terminating condition". In our example |
| the terminating condition is (null? (cdr ls)).   Generally, whenever a recursive |
| procedure is defined, a  terminanting condition must be included so that the re- |
| cursion will eventually stop.                                                    |
| We usually begin the definition of a recursive procedure by writing the termina- |
| ting condition as the first cond clause.We then proceed with the rest of the de- |
| finition.                                                                        |
|                                                                                  |
| In the preceding discussion we introduced the substitution model. Using this mo- |
| del,  we can determine the value of an expression by substituting values for pa- |
| rameters.   There will be times when the substitution model does not work and we |
| use the environment model. In that approach we just remember the bindings of va- |
| riables and avoid any substitutions.                                             | 
|                                                                                  |
| Let us define a procedure "member?"  that decides for us whether its first argu- |
| ment is equal?  to one of the top-level items in the list that is its second ar- |
| gument. For example:                                                             |
|                                                                                  |
| 1. (member? 'cat '(dog hen cat pig))                => #t                        |
| 2. (member? 'fox '(dog hen cat pig))                => #f                        |
| 3. (member? 2 '(1 (2 3) 4))                         => #f                        |
| 4. (member? '(2 3) '(1 (2 3) 4))                    => #t                        |
| 5. (member? 'cat '())                               => #f                        |
|                                                                                  |
| We begin the definition of "member?" by determining the base case. Regardless of |
| what item is, if ls is the empty list, #f is returned. This is the simplest case |
| and will be taken as our base case. To test for the base case, we use the predi- |
| cate null? so the terminating condition is (null? ls).    The consequent for the |
| terminating condition is #f. We can therefore begin the definition of member? as |
| a procedure having two paramters, item and ls:                                   |
|                                                                                  |
|                   (define member?                                                |
|                     (lambda (item ls)                                            |
|                       (cond                                                      |
|                         ((null? ls) #f)                                          |
|                         ...)))                                                   |
|                                                                                  |
| Now given any list,  what is the simplifying operation that simplifies ls to the |
| empty list? It is again the procedure cdr.Assume that ls is not empty.If we know |
| the value of (member? item (cdr ls)), how do we get the value for the expression |
| (member? item ls)?                                                               |
| Well, when is the latter statement true?  It is true if either the first item in |
| ls is the same as item of if item is a member of  the rest of the list following |
| the first item. This can be written as the or expression:                        |
|                                                                                  |
|                   (or (equal? (car ls) item) (member? item (cdr ls)))            |
|                                                                                  |
| Thus in the case when ls is not empty, the above expression is true exactly when |
| the expression (member? item ls) is true. We then complete the definition of the |
| procedure member? with:                                                          |
|#

(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     (else (or (equal? (car ls) item)
	       (member? item (cdr ls)))))))

#|
| The procedure member? is recursive since it calls itself. Let us review the rea- |
| soning used in the program for member?. If the terminating condition  (null? ls) |
| is true, then item is not in ls, and the consequent is false.  Otherwise we look |
| at the alternative,  which is true if either  item is the first item in ls or if |
| item is in (cdr ls) and is otherwise false.                                      |
| When member? calls itself with argument (cdr ls),  its parameter is bound to the |
| value if (cdr ls),  which is a shorter  list than the parameter's previous bind- |
| ding to ls.    In each successive recursive procedure call, the list is shorter, |
| and  the  process is guaranteed to stop  because of the  terminating  condition: |
| (null? ls).                                                                      |
| In order to use a list as the first argument to  member?  we used the  predicate |
| equal? to make the sameness test in the else clause.If we know that the items to |
| which item is bound will always be symbols,  we can use eq? in place  of equal?. |
| The procedure so defined using eq? is named memq? to distinguish it from member? |
| which is defined using equal? for the sameness test. Similarly,  if we know that |
| the items to which item is bound will always be either symbols or numbers,we can |
| use eqv? for the sameness test and call the procedure so defined memv? (*)       |
|                                                                                  |
| We have now defined the procedure last-item, which picks the last top-level item |
| out of a list, and the procedure member?,  which tests whether an item is a top- |
| level element in a given list.  We continue illustrating how to define recursive |
| procedures  with  the definition  of another useful  procedure for  manipulating |
| lists.  The procedure  "remove-1st" removes  the first top-level occurrence of a |
| given item from a list of items. For exmaple:                                    |
|                                                                                  |
| 1. (remove-1st 'fox '(hen fox chick cock))      => (hen chick cock)              |
| 2. (remove-1st 'fox '(hen fox chick fox cock))  => (hen chick fox cock)          |
| 3. (remove-1st 'fox '(hen (fox chick) cock))    => (hen (fox chick) cock)        |
| 4. (remove-1st 'fox '())                        => ()                            |
| 5. (remove-1st '(1 2) '(1 2 (1 2) ((1 2))))     => (1 2 ((1 2)))                 |
|                                                                                  |
|   (*) Note: Scheme provides the three procedures "member", "memq", and "memv",   |
|             written without the question mark. These behave somewhat differen-   |
|             tly from the ones we defined with the question mark in  that if i-   |
|             tem is not found, false is returned,but if the item is found in ls   |
|             the sublist whose car is item is returned. For example:              |
|             (memq 'b '(a b c))   => (b c)                                        |
|                                                                                  |
| In general,  the procedure remove-1st takes two arguments, an element item and a |
| list ls.It builds a new list from ls with the first top-level occurrence of item |
| removed from it. We again begin looking at the simplest case, in which ls is the |
| empty list.Since item does not occur at all in the empty list, the list we build |
| is still the empty list.  The test for the base case is then (null? ls), and the |
| value returned in its consequent is "()".   Thus the definition of the procedure |
| remove-1st begin with:                                                           |
|                                                                                  |
|                   (define remove-1st                                             |
|                     (lambda (item ls)                                            |
|                       (cond                                                      |
|                         ((null? ls) '())                                         |
|                         ... )))                                                  |
|                                                                                  |
| If "ls" is not empty, the procedure that simplifies it to the base case is again |
| cdr. If we already know  (remove-1st item (cdr ls)), that is, if we have a list, |
| consisting of the first top-level occurrence of item removed form  (cdr ls), how |
| do we build up a list that is  obtained by removing the  first top-level  occur- |
| rence of item in ls?.  There are two cases to consider. Let's first consider the |
| example in which we remove the  first occurrence of "a" from the list (a b c d). |
| Since "a" is the first item in the list, we get the desired result by merely ta- |
| king the cdr of the original list.  This is the first case  we consider.  If the |
| first top-level item in ls is the same as  item, then we get the desired list by |
| simply using (cdr ls). This case can be added to the definition of remove-1st by |
| writing                                                                          |
|                                                                                  |
|                   (define remove-1st                                             |
|                     (lambda (item ls)                                            |
|                       (cond                                                      |
|                         ((null? ls) '())                                         |
|                         ((equal? (car ls) item) (cdr ls))                        |
|                         ... )))                                                  |
|                                                                                  |
| The only case left to be considered is when "ls" is not empty, and its first top |
| level item is not the same as "item". Consider the example in which we apply the |
| procedure "remove-1st" to remove the letter c from the list (a b c d).  The list |
| is not empty and its first item is not c. Thus the list we build begins with "a" |
| and continues with the items in (b d).But (b d) is just the list obtained by re- |
| moving c from (b c d).   The final result is then (a b d), which was obtained by |
| building the list                                                                |
|                                                                                  |
|                   (cons (car '(a b c d)) (remove-1st 'c (cdr '(a b c d))))       |
|                                                                                  |
| In general, the list we are building now begins with the first element of ls and |
| has in it the elements of  (cdr ls)  with the first top-level occurrence of item |
| removed.                                                                         |
| But this is obtained when we cons (car ls) onto  (remove-1st item (cdr ls)),  so |
| the final case is disposed of by adding the else clause to the definition, which |
| is given below                                                                   |
|#

(define remove-1st
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     (else (cons (car ls) (remove-1st item (cdr ls)))))))

#|
| To get a better understading of how recursion works, let's walk through the eva- |
| luation of the procedure "remove-1st"; for exmaple  (remove-1st 'c '(a b c d))   |
| Since the list (a b c d) is not empty and the first entry is not c, the alterna- |
| tive in the else clause is evaluated. This gives us                              |
|                   (cons 'a (remove-1st 'c '(b c d)))                             |
| To get the value of this expression, we must evaluate the remove-1st  subexpres- |
| sion. Once again, the list (b c d) is not empty, and the first  item in the list |
| is not the same as c. Thus the alternative in the else caluse is evaluated. This |
| gives us as the value of the whole expression above:                             |
|                   (cons 'a (cons 'b (remove-1st 'c '(c d))))                     |
| Once again, to get the value of this expression, we must evaluate the remove-1st |
| subexpression. Now the list (c d) is not empty, but it first item IS the same as |
| c. Thus the condition in the second cond clause in the definition of remove-1st, |
| is true and the value of its consequent is (d).Thus the above expression has the |
| value:                                                                           |
|                   (cons 'a (cons 'b '(d)))                                       |
| which can be simplified to give the value                                        |
|                   (a b d)                                                        |
| This is the value returned by the procedure call.  In the next section, we shall |
| see how the computer can help us walk through a  procedure application. In order |
| to be able to remove a sublist from a given list,  the predicate equal? was used |
| to test for sameness in the second cond clause. If we know that all of the argu- |
| ments to which item will be bound are symbols,  we can use eq? to test for same- |
| ness. The procedure defined using eq? instead of equal? is named "remq-1st". Si- |
| milarly, if we restrict the arguments to which item will be  bound to symbols or |
| numbers, we can use eqv? to test for sameness in the second cond  clause, and we |
| name the procedure so defined "remv-1st".                                        |
|                                                                                  |
| Exercises                                                                        |
+----------------------------------------------------------------------------------+
| [2.10] Rewrite the definitions of the three procedures "last-item" "member?" and |
|        "remove-1st" with the cond expression replaced by if expressions.         |
|#

(define last-item-if
  (lambda (ls)
    (if (null? (cdr ls))
	(car ls)
	(last-item (cdr ls)))))

(define member-if?
  (lambda (item ls)
    (if (null? ls)
	#f
	(or (equal? (car ls) item)
	    (member-if? item (cdr ls))))))

(define remove-1st-if
  (lambda (item ls)
    (if (null? ls)
	'()
	(if (equal? (car ls) item)
	    (cdr ls)
	    (cons (car ls) (remove-1st-if item (cdr ls)))))))


#|
| [2.11] The definition of "member?" given in this section uses an "or" expression |
|        in the "else" clause. Rewrite the definition of "member?" so that each of |
|        the  two subexpressions  of the "or" expression is  handled in a separate |
|        cond clause.   Compare the resulting definition with the definition given |
|        above of "remove-1st". 
|#



#|
| [2.12] The following procedure, named mystery, takes as its argument a list that | 
|        contains at least two-top level items.                                    |
|#

(define mystery
  (lambda (ls)
    (if (null? (cddr ls))
	(cons (car ls) '())
	(cons (car ls) (mystery (cdr ls))))))

#|
|        What is the value of (mystery '(1 2 3 4 5))? Describe the behavior of the |
|        procedure mystery. Suggest a good name for the procedure.                 |
|                                                                                  |
|        The best name for the procedure  "mystery" is "drop" as it drops the last |
|        element of a list. What the procedures does is:                           |
|        1. Checks if the list has at least two elements. If it has exactly two e- |
|           elements, then gives the first element as result.                      |
|        2. If the list  has more than  two elements conses the  first element and |
|           calls the procedure again but with the could'r.                        |
|                                                                                  |
| [2.13] Define a procedure "subst-1st" that takes three parameters: an item "new" |
|        an item "old" and a list of items "ls". The procedure looks for the first |
|        top-level occurrence of  the item "old" in "ls" and  replaces it with the |
|        item new. Test your procedure on:                                         |
|                                                                                  |
|                 (subst-1st 'dog 'cat '(my cat is clever))  => (my dog is clever) |
|                 (subst-1st 'b 'a '(c a b a c)              => (c b b a c)        |
|                 (subst-1st '(0) '(*) '((*) (1) (*) (2)))   => ((0) (1) (*) (2))  |
|                 (subst-1st 'two 'one '())                  => ()                 |
|                                                                                  |
|        In order to be able to include  lists as possible arguments  to which the |
|        parameters "new" and "old"  are bound, use "equal?" to test for sameness. |
|        Also define procedures "substq-1st" and "substv-1st" that use  "eq?"  and |
|        "eqv?" respectively, instead of "equal?" to test for sameness.            |
|#



     



