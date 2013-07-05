(defun english-letters ()
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K
    #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
    #\W #\X #\Y #\Z))

(defun english-leave-score (bag-size leave)
  "Scoring for a rack leave. Lower is better. Biased against duplicate letters."
  (if (= bag-size 0)
      (if (null leave)
	  0 ; this is a game ending move
	  1 ; we don't care about our leave otherwise
	  )
      (let ((score 0))
	(dolist (x (compress leave))
	  (if (not (or (eql (car x) #\?)
		       (eql (car x) #\F)))
	      (case (cdr x)
		(0)
		(1)
		(2 (setf score (+ score 1)))
		(3 (setf score (+ score 4)))
		(otherwise (setf score (+ score 16))))))
	(values score))))
