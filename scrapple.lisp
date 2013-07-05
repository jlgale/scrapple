; scrapple ruleset

(defun make-scrapple-board ()
  (make-board
   :array (board-connect
	   (make-array
	    '(15 15) :initial-contents
	    (map 'list #'make-board-row
		 '((3xW --- --- 2xL --- --- --- 3xW --- --- --- 2xL --- --- 3xW)
		   (--- 2xW --- --- --- 3xL --- --- --- 3xL --- --- --- 2xW ---)
		   (--- --- 2xW --- --- --- 2xL --- 2xL --- --- --- 2xW --- ---)
		   (2xL --- --- 2xW --- --- --- 2xL --- --- --- 2xW --- --- 2xL)
		   (--- --- --- --- 2xW --- --- --- --- --- 2xW --- --- --- ---)
		   (--- 3xL --- --- --- 3xL --- --- --- 3xL --- --- --- 3xL ---)
		   (--- --- 2xL --- --- --- 2xL --- 2xL --- --- --- 2xL --- ---)
		   (3xW --- --- 2xL --- --- --- 2*W --- --- --- 2xL --- --- 3xW)
		   (--- --- 2xL --- --- --- 2xL --- 2xL --- --- --- 2xL --- ---)
		   (--- 3xL --- --- --- 3xL --- --- --- 3xL --- --- --- 3xL ---)
		   (--- --- --- --- 2xW --- --- --- --- --- 2xW --- --- --- ---)
		   (2xL --- --- 2xW --- --- --- 2xL --- --- --- 2xW --- --- 2xL)
		   (--- --- 2xW --- --- --- 2xL --- 2xL --- --- --- 2xW --- ---)
		   (--- 2xW --- --- --- 3xL --- --- --- 3xL --- --- --- 2xW ---)
		   (3xW --- --- 2xL --- --- --- 3xW --- --- --- 2xL --- --- 3xW)))))))
  
(defun scrapple-en-letter-points (letter)
  (case letter
    ((#\A #\E #\I #\L #\N #\O #\R #\S #\T #\U) 1)
    ((#\D #\G) 2)
    ((#\B #\C #\M #\P) 3)
    ((#\F #\H #\V #\W #\Y) 4)
    (#\K 5)
    ((#\J #\X) 8)
    ((#\Q #\Z) 10)
    (otherwise 0)))

(defun make-scrapple-en-bag ()
  (make-bag :tiles
	    (uncompress 
	     '((#\A . 9)
	       (#\B . 2)
	       (#\C . 2)
	       (#\D . 4)
	       (#\E . 12)
	       (#\F . 2)
	       (#\G . 3)
	       (#\H . 2)
	       (#\I . 9)
	       (#\J . 1)
	       (#\K . 1)
	       (#\L . 4)
	       (#\M . 2)
	       (#\N . 6)
	       (#\O . 8)
	       (#\P . 2)
	       (#\Q . 1)
	       (#\R . 6)
	       (#\S . 4)
	       (#\T . 6)
	       (#\U . 4)
	       (#\V . 2)
	       (#\W . 2)
	       (#\X . 1)
	       (#\Y . 2)
	       (#\Z . 1)
	       (#\? . 2)))))

(defun scrapple-en-rules ()
  (make-rules :letters (english-letters)
	      :letter-points #'scrapple-en-letter-points
	      :make-board #'make-scrapple-board
	      :make-bag #'make-scrapple-en-bag
	      :leave-score #'english-leave-score
	      :rack-size 7))
