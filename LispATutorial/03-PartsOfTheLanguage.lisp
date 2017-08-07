;; Chapter 03: The Parts of the Language
;; -------------------------------------
;;
;; Data Types
;; ----------
;; Common Lisp supports literally an infinite number of data types. While there is
;; a finite number of predefined types, you can define as many other types as you
;; need.
;; You can check the type of an argument using the "type-of" function.
;;
;; (type-of 7)                => FIXNUM
;; (type-of 7.2)              => FLOAT
;; (type-of "abc")            => (SIMPLE-STRING 3)
;; (type-of '(a b b))         => CONS
;; (type-of '#(a b))          => (SIMPLE-VECTOR 2)
;; (type-of #\a)              => STANDARD-CHAR
;;
;; "type-of" returns the most specific type it can.
;; If yuo know the type, or want to determine if an object is a particular type,
;; you can use the "typep" function.
;;
;; (typep 7 'number)           => T
;; (typep '(a b c) 'list)      => T
;; (typep 7/2 'rational)       => T
;; (typep 7/3 'string)         => NIL
;;
;; The "typep" function returns T for an object not only if it is the specified type,
;; but also if the object is a subtype of the one specified.
;;
;; The "subtypep" function determines if one type is a subtype of another type.
;;
;; (subtypep 'integer 'number)   => T
;;                                  T
;; (subtypep 'vector 'array)     => T
;;                                  T
;; (subtypep 'list 'number)      => NIL
;;                                  T
;;
;; Two values are returned. The second value indicates whether or not "subtypep" could
;; determine a relationship between the two types. If the value is T, then the first
;; value indicates whether or not the first type is a subtype of the second type. To
;; determine if one object is a subtype of a type, you can use "type-of" and "subtypep"
;; together.
;;
;; (subtypep (type-of 7) 'number)        => T
;;                                          T
;; (subtypep (type-of '(a b c)) 'list)   => T
;;                                          T
;;
;; Numbers
;; -------
;; Lists may be the heart of Common Lisp, but real-world problems still deal with numbers.
;; For these problems, Lisp provides four types of numbers: integers, ratios, floating-point
;; numbers, and complex numbers.
;;
;; Integers: Integers represent the mathematical integers 0,1,2,3,... and -0,-1,-2,...The
;;           largest integer is most-positive-fixnum and is limited by the implementation.
;;           Integers are usually represented in base 10; however, any other radix can be
;;           used. The radix is shown in the following manner:
;;                                                              #nnrdddd
;;           Where nn is the radix and dddd is the number. The # and r are used to indicate
;;           a radix. For example:
;;                                       #2r1001       => radix-2 number 1001
;;                                       #16ra4b3      => radix-16 number a4b3
;;           Note: # is a macro character. It informs Lisp that the character that follows
;;                 has a special meaning.
;;           Three radices are used frequently enough that they have a special notation.
;;           #b is binary, equivalent to #2r
;;           #o is octal, equivalent to #8r
;;           #x is hexadecimal, equivalent to #16r
;;
;; Ratios: Ratios are numbers represented by two integers, a numerator and a denominator.
;;         Some ratios, such as 4/2, represent integers. Other ratios, such as 1/3, cannot
;;         be represented as integers. The set of all ratios and integers make up the rational
;;         data type. A ratio consists of a sign, a positive numerator, a /, and a positive
;;         denominator. The denominator cannot be signed and must be greater than zero.
;;         For example:
;;         4/5
;;         -5/2
;;         A ratio may contain a radix indicator: #xa/b
;;         The functions "numerator" and "denominator" return the numerator and denominator
;;         of a ratio.
;;         (numerator 4/6)    => 4
;;         (denominator 4/6)  => 6
;;
;; Floating-Point: There are four types of floating-point numbers: long, short, single, and double.
;;                 Each type can have a different precision. A floating-point number consists of
;;                 a sign, an integer, a decimal point, and at least one digit following the
;;                 decimal point. No radix is allowed.
;;
;; 
