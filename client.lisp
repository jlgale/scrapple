; Functions for managing a game client.

(defstruct game-client
  rules			      ; Rules in effect.
  board			      ; Current board of play.
  bag-size		      ; Count of letters remaining in the bag.
  )

(defun game-client-new (rules)
  (make-game-client :rules rules
		    :board (funcall (rules-make-board rules))
		    :bag-size (length (bag-tiles (funcall (rules-make-bag rules))))))

(defun game-client-plays (client dict rackstring)
  (plays (game-client-rules client) dict
	 (game-client-board client) (sort-letters (string-to-list rackstring))))

(defun game-client-play-< (client a b)
  "True if b is a better play than a.  Priority is for a good rack leave."
  (let ((lsa (funcall (rules-leave-score (game-client-rules client))
		      (game-client-bag-size client) (play-leave a)))
	(lsb (funcall (rules-leave-score (game-client-rules client))
		      (game-client-bag-size client) (play-leave b))))
    (if (= lsa lsb)
	(< (play-score a) (play-score b))
	(> lsa lsb))))

(defun game-client-play-square (client square dir word)
  (let* ((used (play-word-filter square word dir))
	 (bag-size (game-client-bag-size client)))
    (square-play (game-client-rules client) square dir word)
    (setf (game-client-bag-size client)
	  (max (- bag-size (length used)) 0))
    (game-client-board client)))
  
(defun game-client-best-play (client dict rackstring)
  "Choose the best play from the given dictionary for the given rack."
  (let ((plays (game-client-plays client dict rackstring)))
    (format t "; found ~a plays~%" (length plays))
    (if (null plays)
	nil
	(let ((best (car plays)))
	  (dolist (p (cdr plays))
	    (if (game-client-play-< client best p)
		(setq best p)))
	  (values
	   (game-client-play-square client (play-square best)
				    (play-direction best) (play-word best))
	   best)))))

(defun game-client-play (client pos wordstring)
  "Notify the client that word was played at pos.  Returns points scored."
  (let ((square (board-ref (game-client-board client) (play-column pos) (play-row pos)))
	(dir (play-direction pos))
	(word (string-to-list wordstring)))
    (game-client-play-square client square dir word)))
    
