; Functions for managing a game server.
;
; XXX - unfinished

(defstruct (player (:print-object player-print))
  nick
  (score 0))

(defun player-print (player stream)
  (format stream "~A ~a ~A"
	  (player-nick player)
	  (list-to-string (player-rack player))
	  (player-score player)))

(defstruct game
  rules
  board
  players
  bag-size
  log)

(defun game-next (game)
  (car (game-players game)))

(defun game-play-< (game a b)
  (let ((lsa (funcall (rules-leave-score (game-rules game)) game (play-leave a)))
	(lsb (funcall (rules-leave-score (game-rules game)) game (play-leave b))))
    (if (= lsa lsb)
	(< (play-score a) (play-score b))
	(> lsa lsb))))

(defun game-plays (game dict)
  (plays (game-rules game) dict (game-board game)
	 (player-rack (game-next game))))

(defun game-best-play (game dict)
  (let ((plays (game-plays game dict)))
    (format t "; found ~a plays~%" (length plays))
    (if (null plays)
	nil
	(let ((best (car plays)))
	  (dolist (p (cdr plays))
	    (if (game-play-< game best p)
		(setq best p)))
	  best))))

(defstruct (log-entry (:print-object log-entry-print))
  nick
  action
  score)
  
(defun log-entry-print (log stream)
  (format stream "~A ~A ~A"
	  (log-entry-nick log)
	  (log-entry-action log)
	  (log-entry-score log)))

(defun game-leader (game)
  (let ((leader (car (game-players game))))
    (dolist (player (cdr (game-players game)))
      (if (> (player-score player)
	     (player-score leader))
	  (setf leader player)))
    leader))

(defun game-event (game player action points leave)
  (let ((log (make-log-entry
	      :nick (player-nick player)
	      :rack (player-rack player)
	      :action action
	      :score (+ (player-score player) points))))
    (setf (player-score player) (+ (player-score player) points))
    (setf (game-log game) (cons log (game-log game)))
    (setf (player-rack player) (sort-letters (fill-rack leave (game-bag game))))
    log))

(defun game-turn (game action points leave)
  (let* ((player (game-next game))
	 (log (game-event game player action points leave)))
    (if (null (player-rack player))
	(values log (game-event game player "game over" (game-end-bonus game) nil))
	(progn
	  (setf (game-players game)
		(append (cdr (game-players game)) (cons player nil)))
	  (values log)))))

(defun check-play-leave (game play)
  (let ((letters (play-word-filter (play-square play)
				   (play-word play) (play-dir play)))
	(rack (player-rack (game-next game))))
    (dolist (l letters)
      (if (lower-case-p l)
	  (setf l #\?))
      (if (member l rack)
	  (setf rack (remove l rack :count 1))
	  (error "playing a letter we don't have: ~A ~A (~A)" l rack play)))
    (if (not (equal rack (play-leave play)))
	(error "play leave doesn't match: ~A vs ~A" play rack))
    (format t "; ~A -> ~A -> ~A matches ~A~%"
	    (player-rack (game-next game)) letters rack play)))

(defun game-end-bonus (game)
  (let ((bonus 0))
    (dolist (player (cdr (game-players game)))
      (dolist (letter (player-rack player))
	(setf bonus (+ bonus (funcall (rules-letter-points (game-rules game)) letter)))))
    bonus))

(defun game-bag-emptyp (game)
  (= (game-bag-size game) 0))

(defun game-play (game play)
  (check-play-leave game play)
  (let ((points (square-play (game-rules game) (play-square play)
			     (play-dir play) (play-word play))))
    (if (not (eq points (play-score play)))
	(error "point tally mismatch for ~A: ~A vs ~A" play points (play-score play)))
    (game-turn game play (play-score play) (play-leave play))))
  
(defun game-add-player (game nick &optional (rack nil))
  (if (null rack)
      (setf rack (pick-letters (game-bag game)
			       (rules-rack-size (game-rules game))))
      (bag-remove-rack (game-bag game) rack))
  (setf (game-players game) (append (game-players game)
				    (cons (make-player :nick nick :rack (sort-letters rack)) nil))))

(defun new-game (rules &rest players)
  (let ((g (make-game :rules rules
		      :board (funcall (rules-make-board rules))
		      :bag (funcall (rules-make-bag rules)))))
    (dolist (p players)
      (game-add-player g p))
    (values g)))

(defun game-pass (game)
  (game-turn game "pass" 0 (player-rack (car (game-players game)))))

(defun game-exchange (game letters)
  (let* ((player (car (game-players game)))
	 (without (player-rack player)))
    (dolist (letter letters)
      (setf without (remove letter without :count 1)))
    (format t "letters: ~A without: ~A~%" letters without)
    (if (not (eq (+ (length without) (length letters)) (rack-size)))
	(error "can't exchange what you don't have!"))
    (game-turn game "exchange" 0
	       (bag-exchange (game-bag game) letters without))))

(defun game-reset (game)
  (setf (game-board game) (funcall (rules-make-board (game-rules game))))
  (setf (game-bag-size game) (length (funcall (rules-make-bag (game-rules game)))))
  (setf (game-log game) nil)
  (dolist (p (game-players game))
    (setf (player-score p) 0)
    (setf (player-rack p) nil)))

(defun game-start (game)
  (dolist (p (game-players game))
    (game-turn game "rack" 0 nil)))

(defstruct game-client
  game
  rack
  bag_size)

(defstruct game-server
  game
  racks
  bag)

(defun game-client-new (rules &rest players)
  (make-game-server :game (apply #'new-game (cons rules players))
		    :rack nil))

(defun game-client-rack (client rackstring)
  (setf (game-client-rack client) (string-to-list rackstring)))

(defun game-client-plays (client dict)
  (plays (game-rules (game-client-game client)) dict
	 (game-board (game-client-game client) (game-client-rack client))))

(defun game-client-best-play (client dict)
  (let ((plays (game-client-plays game dict)))
    (format t "; found ~a plays~%" (length plays))
    (if (null plays)
	nil
	(let ((best (car plays)))
	  (dolist (p (cdr plays))
	    (if (game-play-< (game-client-game client) best p)
		(setq best p)))
	  best))))

(defun game-client-play (client pos wordstring)
  (let* ((square (board-ref board (play-column pos) (play-row pos)))
	 (dir (play-direction pos))
	 (word (string-to-list wordstring))
	 (used (play-word-filter square word dir))
	 (points (square-play (game-rules (game-client-game client))
			      square dir word))
	 (bag-size (game-bag-size (game-client-game client))))
    (setf (game-bag-size (game-client-game client))
	  (max (- bag-size (length used)) 0))
    points))



  
