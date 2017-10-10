#|

    FILENAME:     003-a-simple-database.lisp
    AUTHOR:       Ignacio Sniechowski
    DATE:         01/10/2017
    REVISION:     10/10/2017
    DESCRIPTION:  Taken from "Practical Common Lisp", Chapter 3.
    SUMMARY:      [01] Introduction
                  [02] Filling CDs
                  [03] Looking at the Database Contents
                  [04] Improving the User Interaction
                  [05] Saving and Loading the Database
                  [06] Querying the Database
                  [07] Keyword Parameters
                  [08] Going Back to Out Database Application
                  [09] Updating Existing Records - Another Use for "Where"
                  [10] Removing Duplication and Winning Big 
                  [11] Wrapping Up


    CHAPTER 03 : A SIMPLE DATABASE
    ------------------------------

    [01] INTRODUCTION
    -----------------
    In this chapter we'll write a simple database for keeping track of CDs.
    Common Lisp provides three distinct kinds of operators: functions, macros, and 
    special operators.

    Each record in the database will contain the title, the artist, a rating, and a
    flag indicating whether it has been ripped. 
    So we will start by defining a way to represent a single database record (or in
    other words a single CD).

    For now we will use a very simple and basic structure: a list. We can define lists
    with the "list" function.

    > (list 1 2 3)
    (1 2 3)
    
    We could use a four item list, mapping a given position in the list to a given field
    in the record. However, it is more convenient to use a "property list" or "plist".

    A property list is a list where every other element, starting with the first, is a 
    symbol that describes what the next element in the list is. For the symbols that name
    the fields in the CD database, you can use a particular kind of symbol, called a
    "keyword" symbol. A keyword is any name that starts with a colon (:). Here's an 
    example of a plist using the keyword symbols :a, :b, and :c as property names.

    > (list :a 1 :b 2 :c 3)
    (:A 1 :B 2 :C 3)

    The thing that makes plists a convenient way to represent the records in a database
    is the function "getf", which takes a plist and a symbol and returns the value in
    the plist following the symbol, making a plist a sort of poor man's hash table.

    > (getf (list :a 1 :b 2 :c 3) :a)
    1

    > (getf (list :a 1 :b 2 :c 3) :c)
    3

    So, we can easily write a function "make-cd" that takes the four fields as arguments
    and returns a plist representing that CD.
    The word "defun" tells us that this form is defining a new funtion.

|#

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

#|

    The word "defun" tells us that this form is defining a new function. The name
    of the function is "make-cd", after the name comes the parameter list. This
    function has four parameters: title, artist, rating, and ripped. Everything
    after the parameter is the body of the function.

    In this case, the body is just one form.

    > (make-cd "Violator" "Depeche Mode" 9 t)
    (:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T)

    
    [02] FILLING CDs
    ----------------
    A single record, is not a database, we need some larger construct to hold the
    records. For simplicity a list seems like a good choice. Also, for simplicity
    we can use a global variable *db* (which we can define with the "defvar" macro).
    The asterisks (*) in the name are a Lisp naming convention for global variables.

|#

(defvar *db* nil)

#|

    We can use the "push" macro to add items to *db*, but it is probably a good idea
    to abstract things a tiny bit, so we should define a function "add-record" that
    adds a record to the database.

|#

(defun add-record (cd)
  (push cd *db*))


#|

    Now we can use add-record and make-cd together to add CDs to the database:

    > (add-record (make-cd "Violator" "Depeche Mode" 9 t))
    ((:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T))
    
    > (add-record (make-cd "Joshua Tree" "U2" 9 t))
    ((:TITLE "Joshua Tree" :ARTIST "U2" :RATING 9 :RIPPED T)
     (:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T))
    
    > (add-record (make-cd "The Wall" "Pink Floyd" 8 t))
    ((:TITLE "The Wall" :ARTIST "Pink Floyd" :RATING 8 :RIPPED T)
     (:TITLE "Joshua Tree" :ARTIST "U2" :RATING 9 :RIPPED T)
     (:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T))


    The stuff printed by the REPL after each call to add-record is the return
    value, which is the value returned by the last expression in the function
    body, the "push" macro. And the "push" macro returns the new value of the
    variable it's modifying, so we are actually seeing the value of the database
    as it is being updated.


    [03] LOOKING AT THE DATABASE CONTENTS
    -------------------------------------
    We can also see the current value of *db* whenever we want by simply typing
    *db* at the REPL. However this is not a very human friendly way of looking
    at the output. It would be better to have a function "dump-db" that dumps
    the database like this:

    TITLE:    Violator
    ARTIST:   Depeche Mode
    RATING:   9
    RIPPED:   T

    The function looks like this:

|#

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

#|

    This function works by looping over all the elements of *db* with the
    "dolist" macro, binding each element to the variable "cd" in turn. For
    each value of "cd", we use the "format" function to print it.

    The "format" call is a little cryptic; it takes at least two arguments,
    the first being the stream where it sends its output ("t" is the shorthand
    for the stream *standard-output*).
    The second argument to "format" is a format string that can contain both
    literal text and directives telling "format" things such as how to interpolate
    the rest of its arguments.
    "format" directives start with "~" and it understands dozens of directives,
    each with their own set of options.

    The "~a" directive is the aesthetic directive, it means to consume one 
    argument and output it in a human-readable form. This will render keywords
    without the leading ":" and strings without quotation marks.

    > (format t "~a" "Nachito chito")
    Nachito chito
    NIL

    > (format t "~a" :title)
    TITLE
    NIL

    The "~t" directive is for tabulating. So "~10t" tells "format" to emit
    enough spaces to move to the tenth column before processing the next "~a".
    The "~t" directive doesn't consume any arguments.

    > (format t "~a:~10t~a" :artist "Nachito Chito")
    ARTIST: Nachito Chito
    NIL

  
    When "format" sees "~{" the next argument to be consumed must be a list.
    "format" loops over that list, processing the directives between the "~{"
    and "~}", consuming as many elements of the list as needed each time
    through the list.

    In "dump-db", the "format" loop will consume one keyword and one value
    from the list each time through the loop.
    The "~%" directive doesn't consume any arguments but tells "format" to 
    emit a newline.
   
    Then finally, after the "~}" ends the loop, the last "~%" tells "format"
    to emit one more newline to put a blank line between each CD.

    We can even write the "dump-db" function in one line as follows:
|#

(defun dump-db2 ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))


#|

    [04] IMPROVING THE USER INTERACTION
    -----------------------------------
    Our "add-record" function works fine for adding a single record but if you want
    to add a bunch of records it is not very convenient. So we will write a function
    to prompt the user for information about a set of CDs.
|#

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

#|

    We use "format" to emit a prompt; note that there's no "~%" in the format string,
    so the cursor will stay on the same line.
    The call to "force-output" is necessary to ensure that Lisp doesn't wait for a
    newline before it prints the prompt.
    We can read a single line of text with the function "read-line".
    The global variable *query-io* contains the input stream connected to the terminal.
    The return value of "prompt-read" will be the value of the last form, the call to
    "read-line", which returns the string it read (without the trailing newline).

    We can combine our existing "make-cd" funtion with the "prompt-read" to build a 
    function that makes a new CD record from data it gets by prompting for each value
    in turn.

|#

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))


#|

    The function is almost right, except for the fact that "prompt-read" returns a
    string, which if fine for the title and artist fields but not for the rating and
    ripped fields.
    We will implement a quick and dirty validation using the function "parse-integer".

    > (parse-integer (prompt-read "Rating"))

    Unfortunately, the default behavior of "parse-integer" is to signal an error if it
    can't parse an integer out of the string, or if there's any non-numeric junk in the
    string. But it takes an optional keyword argument ":junk-allowed", which allows for
    some chenge in this:

    > (parse-integer (prompt-read "Rating") :junk-allowed t)

    However, there's still a problem; if it can't find an integer amidst all the junk,
    "parse-integer" will return "NIL" rather than a number. 
    Continuing with our quick-and-dirty approach, we will want to call that 0 and
    continue. We will use Lisp's "or" macro, which is a short-circuiting "or"; it 
    takes a series of expressions, evaluates them one at a time, and returns the
    first non-nil value (or "nil" if they're all "nil")

    > (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)

    to get a default value of 0.

    Fixing the code to prompt for Ripped is quite a bit simpler, we can use the CL
    function "y-or-n-p"

    > (y-or-n-p "Ripped [y/n]: ")
    
    This predicate will be the most robust part of our "prompt-for-cd" as the "y-or-n-p"
    will continue to re-prompt the user if something that is not N,n,Y,y is entered.

    Putting all pieces together we can build a reasonable robust "prompt-for-cd"
    function. 
|#

(defun prompt-for-cd2 ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

#|

    Finally, we can finish the "add a bunch of CDs" interface by wrapping the function
    "prompt-for-cd2" in a function that loops until the user is done. We can use the
    simple form of the "loop" macro, which repeatedly executes a body of expressions
    until it's exited by a call to "return".

|#

(defun add-cds ()
  (loop (add-record (prompt-for-cd2))
     (if (not (y-or-n-p "Another? [y/n]: "))
	 (return))))

#|

    Now you can use "add-cds" to add some more CDs to the database.


    [05] SAVING AND LOADING THE DATABASE
    ------------------------------------
    In CL, if we are using a list as data structure, is easy to save that data to
    a file and reloaded it later. Here is a "save-db" function that takes a filename
    as an argument and saves the current state of the database:

|#

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

#|

    The "with-open-file" macro opens a file, binds the stream to a variable,
    executes a set of expressions, and then closes the file. It also makes sure
    the file is closed even if something goes wrong while evaluating the body.

    The list directly after "with-open-file" is not a function call but rather
    part of the syntax defined by the function. It contains the name of the 
    variable that will hold the file stream to which you'll write within the body
    of "with-open-file" (in this case that variable is "out"), a value that must
    be a file name (in this case "filename"), and then some options that control
    how the file is opened.

    Here you specify that you're opening the file for writing with:
            :direction :output
    And that you want to overwrite an existing file of the same name if it exists
    with:
            :if-exists :supersede    

    Once you have the file open, all you have to do is print the contents of the
    database with (print *db* out).
    Unlike "format", "print" prints Lisp ojects in a form that can e read back in
    by the Lisp reader. 

    The macro "with-standard-io-syntax" ensures that certain variables that affect
    the behavior of "print" are set to their standard values. You'll use the same
    macro when you read the data back in to make sure the Lisp reader and printer
    are operating compatibly.

    So, if we evaluate:

    > (save-db "~/my-cds.db")                                    ;; for *nix
    > (save-db "c:/Users/nakki/my-cds.db")                       ;; for Windows


    The function to load the database back in is similar:

|#

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

#|

    This time you don't need to specify :direction in the option to
    "with-open-file", since you want the default of :input.
    Instead of printing, you use the function "read" to read from the stream
    in. 
    This is the same reader user by the REPL and can read any Lisp expression
    you could type at the REPL prompt. However, in this case, you are just
    reading and saving the expression, not evaluating it.
    The "with-standard-io-syntax" macro ensures that "read" is using the same
    basic syntax that "save-db" did when it printed the data.

    The "setf" macro is CL's main assignment operator. It sets its first
    argument to the result of evaluating its second argument. So in "load-db"
    the "*db*" variable will contain the object read from the file. That is,
    the list of lists written by "save-db". You need to be careful about one
    thing, "load-db" clobbers whatever was in "*db*" before the call. So if
    you have added records with "add-record" or "add-cds" that haven't been
    saved with "save-db", you will lose those records.


    [06] QUERYING THE DATABASE
    --------------------------
    Now that we have a way to save and restore the database, soon a lot of
    records will pile up. So we will need a way to find records in the database
    and not just dump the whole thing.
    We would want to be able to write something like:

    > (select :artist "Depeche Mode")

    and get a list of the records where the artist is the one provided in the
    query.

    The function "remove-if-not" takes a predicate and a list and returns a list
    containing only the elements of the original list that match the predicate.
    However, "remove-if-not" does not really remove anyhing, it just simply
    creates a new list, leaving the original one untouched.
    The predicate argument can be any function that accepts a single argument 
    and returns a boolean value: "nil" for false and anything else for true.

    If we want to extract all the even elements from a list of numbers, we could
    use "remove-if-not" as follows:

    > (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
    (2 4 6 8 10)

    In this case, the predicate is the function "evenp", which returns true if
    its argument is an even number. 
    The notation #' is shorthand for "get me the function with the following
    name". Without the #' Lisp would treat "evenp" as the name of a variable
    and look up the value of the variable, not the function.

    You can pass "remove-if-not" an anonymous function. Suppose that "evenp"
    didn't exist, you could write the previous expression as the following:
    
    > (remove-if-not #'(lambda (x) (= 0 (mod x 2)))
                     '(1 2 3 4 5 6 7 8 9 10))
    (2 4 6 8 10)

    In this case, the predicate is this anonymous function:

    (lambda (x) (= 0 (mod x 2)))

    which checks that its argument is equal to 0 modulus 2 (in other words
    if it is even). Likewise, if we wanted to extract only the odd numbers
    using an anonymous function:

    > (remove-if-not #'(lambda (x) (= 1 (mod x 2)))
                     '(1 2 3 4 5 6 7 8 9 10))
    (1 3 5 7 9)

    Note that "lambda" is not the name of the function, it is just an
    indicator that you are defining an anonymous function. Other than
    the lack of a name, a lambda expression looks a lot like a "defun":
    the word lambda is followed by a parameter list, which is followed by the
    body of the function.

    To select an album given an artist using "remove-if-not", you need a
    function that returns true when the artist field of a record is the one
    provided. 
    Remember that we chose the plist representation for the database records
    because the function "getf" can extract named fields from a plist.
    So assuming "cd" is the name of a variable holding a single database 
    record, you can use the expression

    (getf cd :artist)

    to extract the name of the artist.

    The function "equal", when given string arguments, compares them on a
    character by character basis. So

    (equal (getf cd :artist) "Depeche Mode")

    will test whether the artist field of a given CD is equal to "Depeche Mode".
    All you need to do is wrap that expression in a lambda form, to make an
    anonymous function and pass it to "remove-if-not".

    > (remove-if-not 
       #'(lambda (cd) (equal (getf cd :artist) "Depeche Mode")) *db*)
    ((:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T))
        
    Now suppose you want to wrap that whole expression in a function that takes
    the name of the artist as an argument:

|#

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

#|

    Note that the anonymous function (which contains code that won't run until
    it's invoked in "remove-if-not") can refer to the variable "artist". 
    The anonymous function doesn't just save you from having to write a regular
    function, it lets you write a function that derives part of its meaning (the
    value of "artist") from the context in which it's embedded.

    But "select-by-artist" is only one of the kinds of queries you might like
    to support. You could write several more functions, such as:

    "select-by-title"
    "select-by-rating"
    "select-by-title-and-artist"
    ...

    All these functions would be about the same except for the contents of the
    anonymous function. You can, instead, make a more general "select" function
    that takes a functions as an argument.

|#

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

#|

    What happened to the #'? Well, in this case you don't want "remove-if-not"
    to use the function named "selector-fn". You want it to use the anonymous
    function that was passed as an argument to select in the variable
    "selector-fn". Though, the #' comes back in the call to select.

    > (select #'(lambda (cd) (equal (getf cd :artist) "Depeche Mode")))
    ((:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T))

    But that is really gross-looking, so we can wrap up the creating of the
    anonymous function:

|#

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

#|

    This is a function that returns a function and one that references a variable
    that won't exist after "artist-selector" returns.
    If you call "artist-selector" with an argument of "Depeche Mode", you get an
    anonymous function that matches CDs whose :artist field is "Depeche Mode",
    and if you call it with "Radiohead", you get a different function that will
    match against an :artist field of "Radiohead".

    So now, we can rewrite the all to select like this:

    > (select (artist-selector "Depeche Mode"))
    ((:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 9 :RIPPED T))

    Now you just need some more functions to generate selectors. But just as
    you dont't want to have to write "select-by-title", "select-by-rating", etc,
    because they would all be quite similar, you are not going to want to write
    a bunch of nearly identical selector-function generators, one for each field.

    Why not write one general-purpose selector-function generator, a function that
    depending on what arguments you pass it, will generate a selector function
    for different fields or maybe even a combination of fields?

    You can write such a function, but first you need a crash course in a feature
    called "keyword parameters".
    

    [07] KEYWORD PARAMETERS
    -----------------------
    In the functions you've written so far, you've specified a simple list of
    parameters, which are bound to the corresponding arguments in the call to 
    the function. For instance, the following function:

    > (defun foo (a b c)
        (list a b c))

    Has three parameters: a b c, and must be called with three arguments. 
    But sometimes you may want to write a function that can be called with
    varying numbers of arguments. Keyword parameters are one way to achieve this.
    A version of "foo" that uses keyword parameters might look like this:

    > (defun foo (&key a b c)
        (list a b c))

    The only difference is the "&key" at the beginning of the argument list.
    However, the calls to this new "foo" function will look quite different.
    The following are all legal calls:

    > (foo :a 1 :b 2 :c 3)
    (1 2 3)
    > (foo :c 3 :b 2 :a 1)
    (1 2 3)
    > (foo :a 1 :c 3)
    (1 NIL 3)
    > (foo)
    (NIL NIL NIL)

    All these examples show that the value of the variables a, b, and c are
    bound to the values that follow the corresponding keyword. And if a
    particular keyword isn't present in the call, the corresponding variable 
    is set to "NIL".

    Normally if a function is called with no argument for a particular keyword
    parameter, the parameter will have th value "NIL". However, sometimes you'll
    want to be able to distinguish between a NIL that was explicitly passed as the 
    argument to the keyword parameter and the default value NIL.

    To allow this, when you specify a keyword parameter you can replace the simple
    name with a list consisting of the name of the parameter, a default value, and
    another parameter name, called a "supplied-p" parameter.
    The "supplied-p" parameter will be set to true or false depending on whether
    an argument was actually passed for that keyword parameter in a particular
    call to the function. Here's a version of "foo" that uses this feature:

    > (defun foo (&key a (b 20) (c 30 c-p))
        (list a b c c-p))
    
    The same calls from the earlier example yield:
  
    > (foo :a 1 :b 2 :c 3)
    (1 2 3 T)
    > (foo :c 3 :b 2 :a 1)
    (1 2 3 T)
    > (foo :a 1 :c 3)
    (1 20 3 T)
    > (foo)
    (NIL 20 30 NIL)

    [08] GOING BACK TO OUR DATABASE APPLICATION
    -------------------------------------------
    The general selector-function generator, which you can call "where" for
    reasons that will soon become apparent if you are familiar with SQL
    databases, is a function that takes four keyword parameters corresponding
    to the fields in our CD records and generates a selector function that
    selects any CDs that match all the values given to "where".
    For instance, it will let you say things like this:

    > (select (where :artist "Depeche Mode"))

    or this

    > (select (where :rating 10 :ripped nil))

    The function "where" looks like this:
|#

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)   title)  t)
       (if artist   (equal (getf cd :artist)  artist) t)
       (if rating   (equal (getf cd :rating)  rating) t)
       (if ripped-p (equal (getf cd :ripped)  ripped) t))))

#|

    This function returns an anonymous function that returns the logical "and"
    of one clause per field in our CD records. Each clause checks if the 
    appropiate argument was passed in and then either compares it to the value
    in the corresponding field in the CD record or returns "t", Lisp's version
    of truth, if the parameter wasn't passed in.
    Thus, the selector function will return "t" only for CDs that match all the
    arguments passed to "where".
    Note that you need to use a three-item list to specify the keyword parameter 
    ripped because you need to know whether the caller actually passed
 
    :ripped nil

    meaning "Select CDs whose ripped field is nil", or whether they left out 
    :ripped altogether, meaning "I don't care what the value of the ripped field 
    is"


    [09] UPDATING EXISTING RECORDS - ANOTHER USE FOR "WHERE"
    --------------------------------------------------------
    Now that we have a nice and generalized "select" and "where" functions, we
    are in a good position to write the next feature that every database needs:
    a way to update particular records.
    In SQL the "update" command is used to update a set of records matching a
    particular "where" clause. That seems like a good model, especially since
    you have a where-clause generator. In fact, the update function is mostly
    just the application of a few ideas you've already seen: using a passed-in
    selector function to choose the records to update and using keyword arguments
    to specify the values to change.

    The main new bit is the use of a function "mapcar" that maps over a list, in
    this case *db*, and returns a new list containing the results of calling a
    function on each item in the original list.    

|#

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title    (setf (getf row :title)  title))
	       (if artist   (setf (getf row :artist) artist))
	       (if rating   (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

#|

    One other new bit here is the use of "setf" on a complex form such as
    (getf row :title)
    For now we just need to know that it's a general assignment operator that
    can be used to assign lots of "places" other than just variables. Note that
    "setf" and "getf" don't have any special relationship although they have
    similar names.

    For now it's enough to know that after 

    (setf (getf row :title) title)

    the plist referenced by row will have the value of the variable title
    following the property name :title.
    With this "update" function if you decide that you really like "Depeche Mode",
    and that all their albums should be rated 10, you can evaluate the following
    form:

    > (update (where :artist "Depeche Mode") :rating 10)
    NIL

    And it is so.
  
    > (select (where :artist "Depeche Mode"))
    ((:TITLE "Violator" :ARTIST "Depeche Mode" :RATING 10 :RIPPED T))

    You can even add a function to delete entire rows from the database

|#

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

#|

    The function "remove-if" is the complement of "remove-if-not"; it returns
    a list with all the elements that do match the predicate removed. Like 
    "remove-if-not", it does not actually changes the contents of the database.


    [10] REMOVING DUPLICATION AND WINNING BIG
    -----------------------------------------
    

|#

