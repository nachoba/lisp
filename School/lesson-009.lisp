#|
 
    filename:    lesson-009.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        19/09/2017
    revision:    20/09/2017  
    description: Chapter 5 from "Land of Lisp"
                 In this chapter we begin the building of a text game
                 engine.

    contents:    [01]  Introduction
                 [02]  Describing the Scenery with an Association List
                 [03]  Describing the Location
                 [04]  Describing the Paths
                 [05]  How Quasiquoting Works
                 [06]  Describing Multiple Paths at Once
                 [07]  Describing Objects at a Specific Location
                 [08]  Describing Visible Objects
                 [09]  Describing it All
                 [10]  Walking Around in our World
                 [11]  Picking Up Objects
                 [12]  Checking Our Inventory
                 [13]  Summary
 
|#

#|

    [01] INTRODUCTION
    In this lesson we introduce "The Wizard's Adventure Game", in which you
    are a wizard's apprentice. You'll explore the wizard's house.  
    Our game world consists of:
    * The House
    * The Attic, the Ladder, and the Living Room (all inside the House)
    * The Garden
    * The Well, which is inside the Garden.
    
    In this game, we can visit three different locations: a living room, an
    attic, and a garden. Players can move between places using the door and
    the ladder to the attic.

    Wherever the players are, they can interact with various objects around
    them.

    Basic Requirements: Our game code will need to handle a few basic things:
    * Looking Around
    * Walking to different locations
    * Picking up the objects
    * Performing actions on the objects picked up

    When looking around, you will be able to "see" three kinds of things from
    any location:
    * Basic scenery
    * One or more paths to other locations
    * Objects that you can pick up and manipulate
    

    [02] DESCRIBING THE SCENERY WITH AN ASSOCIATION LIST
    The world inside our adventure game is very simple, contains only three
    locations. Let's create a top-level variable, "*nodes*", to contain
    descriptions of the locations that exist in our game:
|#

(defparameter *nodes* '((living-room (you are in a living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				there is a well in fron of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

#|

    This variable contains a list and description of our three locations. In essence,
    the *nodes* variable basically gives us a way to find a piece of data associated
    with a lookup key.
    In this case, the key is the name of the place (living-room, garden, or attic)
    and the data is a text description of the scenery at that place.
    This type of structure is called an "association list", or "alist" for short.

    One unsual thing about our *nodes* variable is that it does not actually contain
    any text strings. We could have written descriptions using quotes. Why we've 
    done that?
    Manipulation of text is not really a fundamental computing concept, and since the
    easiest things to manipulate in Lisp are symbols and lists, most experienced
    lispers will try to focus on these datatypes.



    [03] DESCRIBING THE LOCATION
    Now that the "alist" of our game world is created, we need to define a command
    to describe a location. To do this, we will use the "assoc" funtion to find the
    correct item in the list using a key:

    > (assoc 'gardn *nodes*)
    (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRON OF YOU.))

    Using "assoc", we can easily create the "describe-location" function:
|#

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

#|

    To use this function, we pass in a location and the *nodes* list:

    > (describe-location 'living-room *nodes*)
    (YOU ARE IN A LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)

    Why don't we just reference the *nodes* variable directly form our function
    "describe-location"? Because this function is written in the functional
    programming style. In this style, a function will reference only parameters or
    variables declared in the function itself, and it will do nothing besides return
    a value, which is the description of the location in this case.

    By writing functions that don't reference variables in the "outside world"
    directly and that don't perform any actions other than returning a value, you
    can write code that can easily be tested in isolation.

    
    [04] DESCRIBING THE PATHS
    Now that we have descriptions of each location, we need descriptions of paths
    to other locations as well. We will create a second variable "*edges*", that
    contains the paths that players can take to move between places on our map.
    (We use the term "edges" because that's the proper math term for the lines
    connecting nodes in a graph)    

|#

(defparameter *edges* '((living-room (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))


#|

    Using this structure we can create the "describe-path" function, which builds a
    textual description of a given edge using our symbols system.

|#

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

#|

    > (describe-path '(garden west door))
    (THERE IS A DOOR GOING WEST FROM HERE.)

    This function basically returns a piece of data with small bits of calculated
    information inserted into it. This freature of Lisp, called quasiquoting, allows
    us to create chunks of data that have small pieces of Lisp code embedded in them.


    [05] HOW QUASIQUOTING WORKS
    To enable quasiquoting, you must use a backquote [`] and not a single quote [']
    when switching from code to data mode. The "describe-path" function has just
    such a backquote in it.

    Both the single quote and the backquote in Lisp "flip" a piece of code into data
    mode, but only a backquote can also be "unquoted" using the comma character, to
    flip back into code mode (after all, a comma does look just like an upside-down 
    backquote.

    Here is how the flip-flop in the "describe-path" function works:

    flip ------> flop -------> flip -> flop ------> flip
    `(there is a ,(caddr edge) going   ,(cadr edge) from here.)

    Lisp attempts to make list manipulation as easy as possible.


    [06] DESCRIBING MULTIPLE PATHS AT ONCE
    Now let's use our "describe-path" function to create a more advanced function.
    Since a location may have any number of paths exiting from it, we need a function
    that can generate descriptions for all edges from a given location by looking up
    the location from our data structure of edges:

|#

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

#|

    > (describe-paths 'living-room *edges*)
    (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.)

    This function uses a bunch of new commands. Many programming languages would use
    some kind of for-next loop to run through the edges, and then cram the descriptions
    of each path together using a temporary variable. Lisp uses a much more elegant
    approach.

    The "describe-paths" function takes the following steps:
         1. Find the relevant edges
         2. Convert the edges to descriptions
         3. Join the descriptions

    Let's see each of these steps in detail:
    1. Finding the relevant edges: The first inner part of the "describe-paths"
       function is pretty straightforward. To find the relevant paths and edges 
       leading from the living room, we use "assoc" again to look up the location
       in our list of edges:
       
       > (cdr (assoc 'living-room *edges*))
       ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))

    2. Converting the edges to descriptions: Next, the edges are conerted to
       descriptions. Here is just the code to accomplish this, shown in isolation:

       > (mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))
       ((THERE IS A DOOR GOING WEST FROM HERE.)
       (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))

       The "mapcar" function is used frequently by Lispers. This function takes
       another function and a list, and then applies this function to every number
       of a list. Here is an example:

       > (mapcar #'sqrt '(1 2 3 4 5))
       (1.0 1.4142135 1.7320508 2.0 2.236068)

       This example passes the "sqrt" funtion, along with the (1 2 3 4 5) list into
       "mapcar". As a result, the function generates a list of the square roots of 
       the original numbers by applying "sqrt" to every member of the list and
       creating a new list.

       Functions that take other functions as parameters, such as "mapcar", are
       very useful and a distinguishing feature of Lisp. Such functions are called
       "higher order functions".
       Here is another example:

       > (mapcar #'car '((foo bar) (baz qux)))
       (FOO BAZ)

       The "#" symbol is shorthand for the function operator. The Lisp reader will
       convert the symbol to (function car), so the following expression is equivalent:

       > (mapcar (function car) '((foo bar) (baz qux)))
       (FOO BAZ)

       Common Lisp requires you to use the "function" operator when referring to a
       function as a value directly like this, because the name of a function may
       conflict with other named items in a program, causing unpredictable errors.
       For instance:

       > (let ((car "Honda Civic"))
           (mapcar #'car '((foo bar) (baz qux))))
       (FOO BAZ)

       In this version, the "car" symbol could have two different meanings. The first
       meaning of "car" is that it is a standard function built into Lisp. However,
       we're also creating a local variable named car. But because we prepended the
       word car with #' in our call to "mapcar", there is no confusion about which
       car we are talking about.


       Let's look at the "describe-paths" function again:

       > (defun describe-paths (location edges)
           (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

       Notice how the "append" and "describe-path" functions are passed in as values
       to the "apply" and "mapcar" functions, which are designed to receive and use
       the functions.

       Common Lisp tracks function names differently from variable names. It has
       multiple "namespaces", including one for variables and one for functions.
       Scheme, on the other hand, has only one namespace for both functions and
       variables.

    3. Joining the descriptions: Once we've used "mapcar" to generate a list of
       descriptions for all the paths and edges, we need to combine them into a single
       description. We accomplish this with the "append" function, which joins several
       lists into one big list:

       > (append '(mary had) '(a) '(little lamb))
       (MARY HAD A LITTLE LAMB)

       We use the "append" function to cram the list of path descriptions into one
       list that describes the whole thing in one swoop. The problem is that "append"
       needs all of the lists handed to it as separate parameters.
       In "describe-paths", we have our lists in one big list, not as separate objects
       we can pass as parameters. We don't even know how many paths there may be from
       any given spot.

       The "apply" function solves this problem. You pass it a function and a list of
       objects, and it pretends that the items in the list are separate objects and
       passes them to the given function as such.

       For example, if we have the nested list '((mary had) (a) (little lamb)), the
       apply function will add in that little bit of duct tape needed to make the
       "append" function to work with a single big list:

       > (apply #'append '((mary had) (a) (little lamb)))
       (MARY HAD A LITTLE LAMB)

       WARNING: Since the "apply" function passes each item in a list as an argument
       to the "target" function, you can run into problems when calling it on very
       large lists that have thousands of items or more. You can check the value of
       the "call-arguments-limit" variable in the REPL to see the maximum number of
       allowd arguments to a function. 

       You can see how "apply" enables the "describe-paths" function to build one
       long list describing all paths leading from a single location. Let's use this
       same approach on the path description list we constructed:

       > (apply #'append '((THERE IS A DOOR GOING WEST FROM HERE.)
                           (THERE IS A LADDER GOING UPSTAIRS FROM HERE.)))

       (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FORM
        HERE.)

       
    Now that we've looked at each part of the "describe-paths" function, let's review
    how it works:

    > (defun describe-paths (location edges)
        (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

    The function takes two parameters: the current player's location, as well as an
    alist of edges/paths for the game map.
    First, it uses "assoc" to look up the correct location from the edge alist. Since
    "assoc" returns both the key and the value from the alist, we call "cdr" to 
    retrieve only the value.
    Next, we use "mapcar" to map the "describe-path" function against each edge that
    we found.
    Finally, we concatenate the lists for describing all the paths into one long list
    by applying "append" against the list.



    [07] DESCRIBING OBJECTS AT A SPECIFIC LOCATION
    To create the final piece of code to help us visualize our game world, we need
    to describe the objects on the floor at a given location, which a player can pick
    up and use.

    Listing visible objects: To do so, we first create a list of objects:
|#

(defparameter *objects* '(whiskey bucket frog chain))

#|

    We can also create a second variable, *object-locations*, to track the location
    of each object in the form of an alist:

|#

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

#|

    Next, we write a function that lists the objects visible form a given location:

|#

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)                             ;; (1)
	     (eq (cadr (assoc obj obj-locs)) loc)))    ;; (2)
    (remove-if-not #'at-loc-p objs)))                  ;; (3)

#|

    The "objects-at" function declares a new function named "at-loc-p" using the 
    "labels" command. (1) (Remember that the "labels" function allows you to define
    functions locally). Since the "at-loc-p" function won't be used elsewhere we can
    just declare it directly within "objects-at", hiding it from the rest of the code
    in our program.

    The "at-loc-p" function takes the symbol for an object and returns "t" or "nil",
    depending on whether that object exists at the location "loc". It does this by
    looking up the object in the "obj-locs" alist. Then, it uses "eq" to see whether
    the location it finds matches the location in question. (2)

    Why did we name this function "at-loc-p"? When a function returns "nil" or a
    truth value, it's a Common Lisp convention to append a "p" to the end of that
    function's name. For instance, you can check that the number 5 is odd by calling
    "oddp". Such true/false functions are called "predicates", which is why we use
    the letter "p".

    The "remove-if-not" function in the last line of the listing (3), removes all things
    from a list for which a passed-in function (in this case, "at-loc-p") doesn't
    return true. Essentially, it returns a filtered list of objects consisting of those
    items for which "at-loc-p" is true.

    Here is what the "object-at" looks like in action:

    > (objects-at 'living-room *objects* *object-locations*)
    (WHISKEY BUCKET)


    [08] DESCRIBING VISIBLE OBJECTS
    Now we can write a function to describe the objects visible at a given location:

|#

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)                                                 ;; (1)
	     `(you see a ,obj on the floor.)))                                 ;; (2)
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))   ;; (3)

#|

    "describe-objects" first creates the "describe-obj" function (1). This function
    generates a pretty sentence stating that a given object is on the floor, using
    quasiquoting (2).
    The main part of the function consists of calling "objects-at" to find the objects
    at the current location, mapping "describe-obj" across this list of objects, and
    finally appending the descriptions into a single list (3).

    Let's try running "describe-objects":

    > (describe-objects 'living-room *objects* *object-location*)
    (YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)


    [09] DESCRIBING IT ALL
    Now we'll tie all of these description functions into one easy command called "look".
    Because this will be the actual command players can enter to look around them in the
    game, "look" will need to know a player's current location. 

    So, we need a variable to track the player's current position; we will call this
    variable *location*
|#

(defparameter *location* 'living-room)

#|

    The *location* value is initialized to the "living-room" symbol, which occurs at the
    very start of the game. So players will find themselves in the living room at the
    beginning of the game.

    At this point we can write a "look" function to describe everything we need by having
    it call of our descriptor functions:

|#

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

#|

    Notice that the "look" function use global variables (such as *location*, *nodes*, etc).
    This means that the player won't have to pass any values in order to look out the world.
    However, this also means that the "look" function is not in the functional programming
    style, because functions in this style reference only parameters or variables declared
    in the function itself.

    Since the player's location changes as the game progresses, "look" will do "different
    things" at "different times" in the game. The things you see when looking around will
    change depending on your location.

    In contrast, a function in the functional programming style always returns the same
    result, as long as the same values are given as parameters.

    Let's see "look" in action:

    > (look)
    (YOU ARE IN A LIVING-ROOM.
    A WIZARD IS SNORING LOUDLY ON THE COUCH.
    THERE IS A DOOR GOING WEST FROM HERE. 
    THERE IS A LADDER GOING UPSTAIRS FROM HERE. 
    YOU SEE A WHISKEY ON THE FLOOR. 
    YOU SEE A BUCKET ON THE FLOOR.)


    [10] WALKING AROUND IN OUR WORLD
    Now that we can see things in our world, let's write some code so that we can walk
    around. The "walk" function (not in the functional style) takes a direction and lets
    us walk there:

|#

(defun walk (direction)
  (let ((next (find direction                                          ;; (1)
		    (cdr (assoc *location* *edges*))                   ;; (2)
		    :key #'cadr)))                                     ;; (3)
    (if next                                                           ;; (4)
	(progn (setf *location* (car next))                            ;; (5)
	       (look))                                                 ;; (6)
	'(you cannot go that way.))))                                  ;; (7)

#|

    First, this function looks up the available walking paths in the *edges* table,
    using the current location (2). This is used by the "find" function to locate the
    path marked with the appropriate direction (1).
    "find" searches a list for an item, then returns that found item). The "direction"
    (such as "west", "upstairs", and so on) will be in the cadr of each path, so we 
    need to tell "find" to match the "direction" against the "cadr" of all the paths in
    the list.

    We can do this by passing "find" a keyword parameter (3). In Common Lisp, many
    functions (such as "find") have built-in features that can be accessed by passing
    in special parameters at the end of the function call. For instance, the following
    code finds the first item in a list that has the symbols "y" in the "cadr" location:

    > (find 'y '((5 x) (3 y) (7 z)) :key #'cadr)
    (3 Y)

    A keyword parameter has two parts:

    * The first is the name (in this case :key), which begins with a colon
    * The second is the value, which in this case is the #'cadr

    We use keyword parameters the same way in our "walk" function to find the
    proper path based on the given direction.
    Once we have the correct path, we store the result in the variable "next" (1).
    The "if" expression then checks whether "next" has a value (4), that is, the "next"
    variable isn't "nil". If "next" has a value, "if" adjusts the player's position
    because this is a valid direction (5). The call to "look" (6) retrieves the 
    description for the new location and returns it as a value. If the player chooses
    an invalid direction, "look" will generate an admonishment instead of a new
    description (7).

    Let's see our "walk" function in action:

    > (walk 'west)
    (YOU ARE IN A BEAUTIFUL GARDEN. 
    THERE IS A WELL IN FRON OF YOU. 
    THERE IS A DOOR GOING EAST FROM HERE. 
    YOU SEE A FROG ON THE FLOOR. 
    YOU SEE A CHAIN ON THE FLOOR.)

    There's a quote in front of the direction, since the direction name needs to
    be written in data mode. It's awkward to force a player to put a quote in a
    game command.


    [11] PICKING UP OBJECTS
    Next, let's create a command to pick up objects in our world. To do so, we modify
    the variable *object-locations* that we're using to track the location of objects:
|#

(defun pickup (object)
  (cond ((member object                                                  ;; (1)
		 (objects-at *location* *objects* *object-locations*))   ;; (2)
	 (push (list object 'body) *object-locations*)                   ;; (3)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

#|

    The "pickup" function uses the "member" (1) function to see if the "object" is
    indeed on the floor of the current location.
    The "member" function checks to see if a particular item is found in a list of items.
    We use the "objects-at" command (2) to generate the lists of objects at the current
    location.
    If the object is at the current location, we use the "push" command (3) to push a
    new item onto the *object-locations* list, consisting of the item and its new
    location. The new location will just be "body", for the player's body.
    The "push" command (3) simply adds a new item to the front of a list variable's list.
    For example, the following example adds the number 7 to the list 1 2 3:

    > (defparameter *foo* '(1 2 3))
    *FOO*
    > (push 7 *foo*)
    (7 1 2 3)

    The "push" command is basically a convenience function built on top of "setf". For
    example, we could have replaced the preceding "push" command with:
    
    > (setf *foo* (cons 7 *foo*))
    
    And obtained the same result, but it's easier to use "push".

    Pushing a new location for an object onto our *object-locations* alist seems a bit
    odd. Since we're never removing old locations for objects, just pushing new ones,
    it means that *object-locations* may contain multiple entries for a single object,
    and that this list now has two stored locations for the object in question.
    Fortunately, the "assoc" command, which we use to find objects in a given location (
    whithing the "objects-at" command), always returns the first item it finds in a list.
    Therefore, using the "push" command makes the "assoc" command behave as if the value
    in the list for a given key has been replaced altogether.


    Using the "push" and "assoc" commands together in this way allows us to pretend that
    values in an alist are changing, while still preserving old values. Old values are
    simply suppressed by newer values, thus preserving a history of all old values.
    The "push"/"assoc" idiom is a common technique used by lispers.

    Now let's walk back to the living room and try to pick up an object:

    > (walk 'east)
    (YOU ARE IN A LIVING-ROOM. 
    A WIZARD IS SNORING LOUDLY ON THE COUCH. 
    THERE IS A DOOR GOING WEST FROM HERE. 
    THERE IS A LADDER GOING UPSTAIRS FROM HERE. 
    YOU SEE A WHISKEY ON THE FLOOR. 
    YOU SEE A BUCKET ON THE FLOOR.)


    > (pickup 'whiskey)
    (YOU ARE NOW CARRYING THE WHISKEY)


    [12] CHECKING OUR INVENTORY
    Finally, let's create a function that lets players see an inventory of objects they
    are carrying:
    
|#

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

#|

    This inventory function uses the "object-at" function to retrieve a list of objects
    at a requested location. What location does it search for?
    If you remember, when an object was picked up by the player, we changed its location 
    to "'body". This is the location we now use to query:

    Let's try out this "inventory" function:

    > (inventory)
    (ITEMS- WHISKEY)


   [13] SUMMARY
   * A game world can be represented by a mathematical graph, consisting of "nodes" for the
     places the player can visit and "edges" for the paths between these places.
   * You can store these nodes in an association list "alist" called *nodes*. This alist
     allows you to look up properties of a node/place by using its name. In the case of our
     game, the property we're storing is a description of each node/place.
   * You use the "assoc" function to look up a key (location name in our example) in an alist.
   * "Quasiquoting" is a technique that allows you to insert small bits of computer code into
     larger pieces of data.
   * Some Lisp functions accept other functions as arguments. These are called higher-order
     functions. The "mapcar" function is the most popular higher-order function in CL.
   * To replace a value from an alist, you "push" new items onto the list. Only the most
     recent value will be reported by the "assoc" function.

|#
