; wwf ruleset
(defun wwf-letter-points (letter)
  (case letter
    ((#\A #\E #\I #\O #\R #\S #\T) 1)
    ((#\D #\N #\L #\U) 2)
    ((#\H #\G #\Y) 3)
    ((#\B #\C #\F #\M #\P #\W) 4)
    ((#\K #\V) 5)
    ((#\X) 8)
    ((#\J #\Q #\Z) 10)
    (otherwise 0)))

(defun make-wwf-bag ()
  (make-bag :tiles
	    (uncompress 
	     '((#\A . 9)
	       (#\B . 2)
	       (#\C . 2)
	       (#\D . 5)
	       (#\E . 13)
	       (#\F . 2)
	       (#\G . 3)
	       (#\H . 4)
	       (#\I . 8)
	       (#\J . 1)
	       (#\K . 1)
	       (#\L . 4)
	       (#\M . 2)
	       (#\N . 5)
	       (#\O . 8)
	       (#\P . 2)
	       (#\Q . 1)
	       (#\R . 6)
	       (#\S . 5)
	       (#\T . 7)
	       (#\U . 4)
	       (#\V . 2)
	       (#\W . 2)
	       (#\X . 1)
	       (#\Y . 2)
	       (#\Z . 1)
	       (#\? . 2)))))

(defun make-wwf-board ()
  (make-board
   :array (board-connect
	   (make-array
	    '(15 15) :initial-contents
	    (map 'list #'make-board-row
		 '((--- --- --- 3xW --- --- 3xL --- 3xl --- --- 3xW --- --- ---)
		   (--- --- 2xL --- --- 2xW --- --- --- 2xW --- --- 2xL --- ---)
		   (--- 2xL --- --- 2xL --- --- --- --- --- 2xL --- --- 2xL ---)
		   (3xW --- --- 3xL --- --- --- 2xW --- --- --- 3xL --- --- 3xW)
		   (--- --- 2xL --- --- --- 2xL --- 2xL --- --- --- 2xL --- ---)
		   (--- 2xW --- --- --- 3xL --- --- --- 3xL --- --- --- 2xW ---)
		   (3xL --- --- --- 2xL --- --- --- --- --- 2xL --- --- --- 3xL)
		   (--- --- --- 2xW --- --- --- -*- --- --- --- 2xW --- --- ---)
		   (3xL --- --- --- 2xL --- --- --- --- --- 2xL --- --- --- 3xL)
		   (--- 2xW --- --- --- 3xL --- --- --- 3xL --- --- --- 2xW ---)
		   (--- --- 2xL --- --- --- 2xL --- 2xL --- --- --- 2xL --- ---)
		   (3xW --- --- 3xL --- --- --- 2xW --- --- --- 3xL --- --- 3xW)
		   (--- 2xL --- --- 2xL --- --- --- --- --- 2xL --- --- 2xL ---)
		   (--- --- 2xL --- --- 2xW --- --- --- 2xW --- --- 2xL --- ---)
		   (--- --- --- 3xW --- --- 3xL --- 3xL --- --- 3xW --- --- ---)))))))

(defun wwf-rules ()
  (make-rules :letters (english-letters)
	      :letter-points #'wwf-letter-points
	      :make-board #'make-wwf-board
	      :make-bag #'make-wwf-bag
	      :leave-score #'english-leave-score
	      :rack-size 7))
