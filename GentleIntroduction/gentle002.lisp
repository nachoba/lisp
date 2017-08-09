;; Chapter 02 :: Lists
;; ----------

;; Lists remain the central data type in Lisp. Lists are important because
;; they can be made to represent practically anything: sets, tables, graphs...

;; What do lists look like?
;; ------------------------
;; Every list has two forms: a printed representation and an internal one.
;; The printed representation is most conveninet for people to use. The internal
;; representation is the way the list actually exists in the computer's memory.
;; In its printed form, a list is a bunch of items enclosed in parentheses. These
;; items are called the elements of the list. Here are some examples of lists:
;; (red green blue)
;; (addamant)
;; ()
;; (2 3 4 6 7 8)

;; Internally, lists are organized as chains of "cons cells". The cons cells are
;; linked together by pointers. Each cell has two pointers: one of them always
;; points to an element of the list, while the other points to the next cons cell
;; in the chain.
;; [red | -] --> [green | -] --> [blue | -] --> NIL
;; When the list is written in parenthesis notation, the NIL at the end of the
;; chain is omitted.

;; A symbol and a list of one element are not the same. AARD (AARD)

;; Nested lists: A list may contain other lists as elements:
;; ( (blue sky) (green grass) (brown earth) )
;; This is a list of three lists.

;; Length of lists: The length of a list is the number of elements it has. The
;; primitive function "length" computes the length of a list. It is an error to
;; give "length" a symbol or number as input.

;; NIL: The empty list: A list of zero elements is called an empty list. It has
;; no cons cells. It is written as an empty pair of parentheses: ()

;; Equality of lists: Two lists are considered equal if their corresponding elements
;; are equal.

;; First, Second, Third, and Rest: Lisp provides primitive functions for extracting
;; elements from a list. The functions: first, second, third return the first, second,
;; and third element of their input, respectively.
;; The rest function is the complement of first. It returns a list containing everything
;; but the first element.

;; CAR and CDR
;; -----------
;; By now you know that each half of a cons points to something. These two halves have
;; obscure names. The left half is called the CAR, and the right half is called the CDR
;; (pronounced "cou-der"). Besides naming the two halves of a cons cell, CAR and CDR
;; are also the names of built-in Lisp functions that return whatever pointer is in the
;; CAR or the CDR half of the cell, respectively.
;; * car : returns the first element of the list.
;; * cdr : returns the rest of the list.
;; So you can see that "first" is the same as "car", and "rest" is the same as "cdr".

;; The cdr of a single-element list: Since a list of length one is represented inside
;; the computer as a single cons cell, the CDR of a list of length one is the list of
;; length zero: NIL.

;; CAR and CDR of Nested Lists
;; ---------------------------
;; CAR and CDR can be used to take apart nested list just as easily as flat ones.
;; Suppose we have the list: ( (blue cube) (red pyramid) )

;; CAR / CDR Pronunciation Guide
;; -----------------------------
;; Function          Pronunciation          Alternate Name
;; --------          -------------          --------------
;; CAR               kar                    first
;; CDR               cou-der                rest
;;
;; CAAR              ka-ar
;; CADR              kae-der                second
;; CDAR              cou-dar
;; CDDR              cou-dih-der
;;
;; CAAAR             ka-a-ar
;; CAADR             ka-ae-der
;; CADAR             ka-dar
;; CADDR             ka-dih-der             third
;; CDAAR             cou-da-ar
;; CDADR             cou-dae-der
;; CDDAR             cou-dih-dar
;; CDDDR             cou-did-dih-der
;;
;; CADDDR            ka-dih-dih-der         fourth
;;
;; and so on...

;; So (caar ( (blue sky) (red pyramid) ))  => blue

