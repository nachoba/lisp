# An Implementation of Guess my Number

(setq *small* 1)
(setq *big* 100)


(de guess NIL
  (/ (+ *small* *big*) 2) )

(de smaller NIL
  (setq *big* (- (guess) 1))
  (guess) )

(de bigger NIL
  (setq *small* (+ (guess) 1))
  (guess) )

(de start-over NIL
   (setq *big* 100)
   (setq *small* 1)
   (guess) )


