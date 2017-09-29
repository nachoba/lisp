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



