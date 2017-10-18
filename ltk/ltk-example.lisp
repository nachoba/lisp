;;;;   FILENAME:        ltk-example.lisp
;;;;   AUTHOR:          Ignacio Sniechowski
;;;;   DATE:            18/10/2017
;;;;   REVISION:
;;;;   DESCRIPTION:     Some functions that show how to use the ltk binding
;;;;                    for Tk.


(defun hello ()
  "A simple Hello World application using LTK"
  (with-ltk ()                                                             ;; [1]
    (let ((button (make-instance 'button                                   ;; [2]
				 :master nil
				 :text "Press Me"
				 :command (lambda ()
					    (format t "Hello World!~&")))))
      (pack button))))                                                     ;; [3]

#|

    [1] The whole code of "hello" is wrapped into the "with-ltk" macro. It
    ensures that the GUI library is properly set up and the communication with
    the TK toolkit is established. It runs the contained code which should  
    initialize the GUI and after that calls "mainloop" to process the GUI events.


    [2] The next step is to create the button. This is done by creating an
    instance of the class button. The ":text" argument gives the text to display
    on the button and ":command" is the function to call, whenever the button is
    pressed.

    While the last two arguments are obvious, the ":master" needs explanation.
    In Tk, all GUI elements are arranged in a tree. So every GUI element has a
    parent node or 'master' which designates its position in the tree structure.

    So put there the object that should be the parent for your button. Only for
    top-level components "nil" may be given instead of an object.

    [3] For displaying any ltk object, a layout manager is used. There are two
    layout managers available, which can be used to arrange widgets in its 
    parent: "pack" and "grid".

    "pack" arranges widgets as a heap of boxes, which are horizontally or
    vertically stacked.
    "grid" arranges widgets in a table-like layout.
    Never use "pack" and "grid" for the same container, unpredictable behaviour
    may result.


|#


(defun two-buttons-hello ()
  (with-ltk ()
    (let* ((f  (make-instance  'frame))
	   (b1 (make-instance  'button
			       :master f
			       :text "Button 1"
			       :command (lambda () (format t "Button1~&"))))
	   (b2  (make-instance 'button
			       :master f
			       :text "Button 2"
			       :command (lambda () (format t "Button2~&")))))
      (pack f)
      (pack b1 :side :left)
      (pack b2 :side :left)
      (configure f :borderwidth 3)
      (configure f :relief :sunken))))

#|

    This example shows how to group two buttons within a frame, and configure
    widgets in general.
    The created frame is given as the master parameter to the button creation.
    This automatically ensures that the buttons are packed within the frame.
    To change the appeareance of the Frame f, the configure function is used.
    This is a very generic function, which can be used upon any tk object.
    It takes two arguments, the name of the configuration option and the value
    to set into it. The value can be any tk object or any properly printable
    Lisp object.

    In this case, we set the width of the border of the frame to 3 and make it
    a sunken border. Other styles could be: raised, ridge, groove, flat, solid.

    For a comprehensive list of configuration options look in the manpage of the
    tk widgets as well as man options for options shared by all tk widgets.


|#
