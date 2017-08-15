# A Function to calculate the factorial of a number

(de factorial (x)
   (if (= x 0)
      1
      (* x (factorial (- x 1))) ) )


# As an addition two functions that calculate the head and the tail of a list

(de head (x)
   (car x) )

(de tail (x)
   (cdr x) )



