# Debugging                                                  
# ---------                                                  
                                                            
# Tracing                                                    
# -------                                                    
# This method means letting functions of interest print their
# name and arguments when they are entered, and their name   
# again and return value when they are exited.               

(de fact(n)
   (if (= 0 n)
      1
      (* n (fact (dec n)))))

# After entering the REPL, and loading this file we use trace
# to enter debug-tracing mode:                               

# (trace 'fact)

# Calling the function now (fact 3) will produce:            
#                                                            
# : (fact 3)                                                 
#   fact : 3                                                 
#    fact : 2                                                
#     fact : 1                                               
#      fact : 0                                              
#      fact = 1                                              
#     fact = 1                                               
#    fact = 2                                                
#   fact = 6                                                 
# -> 6                                                       
  

# As can be seen here, each level of function call will in-  
# dent by and additional space. Upon function entry, the name
# is separated from the arguments with a colon ':', and upon 
# function exit with an equal '=' sign from the return value.

# 'trace' works by modifying the function body. To reset a   
# function to its untraced state, call 'untrace'.            

# (untrace 'fact)


# Single-Stepping                                            
# ---------------                                            
# This method means to execute a function step by step, gi-  
# ving the programmer an opportunity to look more closely at 
# what is happening. The function 'debug' inserts a break-   
# point into each top-level expression of a function. When   
# the function is called, it stops at each breakpoint, dis-  
# plays the expression it is about to execute next -this ex- 
# pression is also stored into the global variable ^ - and   
# enters a read-eval-loop. The programmer can then:          




