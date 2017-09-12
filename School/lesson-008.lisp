#|
 
    filename:    lesson-08.lisp
    author:      ignacio sniechowski
    mail:        0800nacho@gmail.com
    date:        12/09/2017
    revision:    
    description: Creating new projects.
                 In this lesson we will introduce the use of quickproject to
                 create new projects.

    contents:    [1] Installing "quickproject"
                 [2] Creating a Project
                 [3] What's inside the directory?
                 [4] Loading the project.
|#


#|

    [1] INSTALLING quickproject
    To install quickproject, start Emacs and do M-x slime to start the REPL.
    After that evaluate:  (ql:quickload "quickproject")
    Quickproject should be installed.

    [2] CREATING A PROJECT
    Using M-x eshell go to ~/quicklisp/local-projects/ and fire a REPL.
    To create a project use (quickproject:make-project "project-name"), where
    "project-name" is the name of your project.
    Check in ~/quicklisp/local-projects/ that a directory with the name of the
    project was created.

    [3] WHAT'S INSIDE THE DIRECTORY?
    You will see the following files:

      README.txt
      package.lisp
      project-name.asd
      project-name.lisp 

      The project-name.asd file defines a "system". A system in CL is kind 
      of a library.
      The project-name.lisp is where your code goes.
      The README.txt is just a text file for you to fill it with information.
      Finally, the package.lisp @TODO

      
    [4] LOADING THE PROJECT
    In the REPL you can load the project by evaluating:
    (ql:quickload :project-name)
    That will load the project-name.lisp file and all dependencies defined.
    You can compile each chunk of code doing C-c C-c in the source file.

|#
