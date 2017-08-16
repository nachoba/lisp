#|
ANSI COMMON LISP
----------------
Donald Knuth called his classic series "The Art of Computer Programming".
In his Turing Award Lecture, he explained that this title was a conscious
choice -that what drew him to programming was "the possibility of writing
beautiful programs".
   Like architecture, programming has elements of both art and science. A
program has to live up to mathematical truth in the same way that a buil-
ding has to live up to the laws of physics. But the architect's aim isn't
simply to make a building that does not fall down. Almost always the real
aim is to make something beautiful.
   Many programmers feel,like Donald Knuth, that this's also the real aim
of programming. Almost all Lisp hackers do.The spirit of Lisp hacking can
be expressed in two sentences. Programming should be fun. Programs should
be beautiful. That's the spirit I have tried to convey in this book.

Introduction
------------
Lisp is the second oldest language still in use. The first implementation
began in 1958 by John McCarthy and his students.  What is more remarkable
is that is still in the forefront of programming language technology.Part
of what makes Lisp distinctive is that it is designed to evolve.  You can
use Lisp to define new Lisp operators. As new abstractions become popular
(object-oriented programming, for example),it always turns out to be easy
to implement them in Lisp.

Lisp lets you do things that you can't do in other programming languages.
For example, if you want to write a function that takes a number "n", and
returns a function that adds "n" to its argument:
|#
(defun addn (n)
  #' (lambda (x)
       (+ x n)))
#|
You can't do that in C, for example. Another unique feature of Lisp, pos-
sibly even more valuable than closures, is that Lisp programs are expres-
sed as Lisp data structures.  This means that you can write programs that
write programs. They are called "macros", and they are used a lot.
With macros, closures, and run-time typing,Lisp trascends object-oriented
programming.

Lisp gives you tools that other languages don't provide: Lisp is designed
to be extensible: it lets you define new operators yourself. This is pos-
sible because the Lisp language is made out of the same functions and ma-
cros as your own programs.  So it's no more difficult to extend Lisp than
to write a program in it. In fact, it is so easy that is a standard prac-
tice.
Almost any  program can benefit from having the language tailored to suit
its needs,  but the more complex the program, the more valuable botoom-up
programming becomes.A bottom-up program can be written as a series of la-
yers,each one acting as a sort of programming language for the one above.
You can write programs bottom-up in any language,but Lisp es far the most
natural vehicle for this style.  Bottom-up programming leads naturally to
extensible software.   If you take the principle of bottom-up programming
all the way to the topmost layer of your program, then that layer becomes
a programming language for the user. Because the idea of extensibility is
so deeply rooted in Lisp,it makes the ideal language for writing extensi-
ble software.
Working bottom-up is also the best way to get reusable software.  The es-
sence of  writing reusable software  is to separate the  general from the
specific, and bottom-up programming inherently creates such a separation.
Instead of devoting all  your effort to writing a single,  monolithic ap-
plication, you devote part of your effort to building a language,and part
to writing a smaller application on top of it.   What is specific to this
specific to this application will be  concentrated in  the topmost layer.
The layers beneath  will form a language  for writing  applications  like
this one -and what could be more reusable than a programming language?
The phrase "rapid prototyping" describes a kind of programming that began
with Lisp:  in Lisp, you can often write a prototype in less time than it
would take to write the spec for one.

One of the aims of this book is to explain not just the Lisp language,but
the new approach to programming that Lisp makes possible. 


