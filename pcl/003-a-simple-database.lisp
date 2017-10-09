#|

    Chapter 03 : A Simple Database

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

    
    FILLING CDs
    -----------
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


    LOOKING AT THE DATABASE CONTENTS
    --------------------------------
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

    IMPROVING THE USER INTERACTION
    ------------------------------
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

|#
