;;;;  FILENAME:

(defpackage :hello-word-ltk
  (:use :common-lisp :ltk)
  (:export #:main))


(in-package :hello-world-ltk)

(defun main ()
  (setf *debug-tk* nil)
  (with-ltk ()
    (let ((b (make-instance
	      'button
	      :text "Hello World!"
	      :command (lambda ()
			 (do-msg "Bye!" "Hello World!")
			 (setf *exit-mainloop* t)))))
      (pack b))))

