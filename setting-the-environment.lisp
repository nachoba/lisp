#|
To set a working environment for Common Lisp development:
1. Download emacs
2. Download sbcl
3. Download quicklisp
4. Add Melpa to Emacs' package manager
5. Install Slime
6. Add (setq inferior-lisp-program "path to sbcl") to .emacs
7. Run quicklisp from within emacs' slime mode
8. Add quicklisp to SBCL init file
|#


(defun greet (name)
  (format t "Hello, ~a, how are you?~%" name))

#|
Once a function is defined doing C-c C-c will compile in the REPL.
|#
