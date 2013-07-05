; Functions for playing words on a board and finding legal plays.
; See board-play and plays functions.

(defun other-direction (dir)
  (ecase dir
    (down 'across)
    (up 'across)
    (across 'down)
    (back 'down)))

(defun reverse-direction (dir)
  (ecase dir
    (down 'up)
    (up 'down)
    (across 'back)
    (back 'across)))

(defun next-position (dir sq)
  (ecase dir
    (down (square-down sq))
    (across (square-across sq))
    (up (square-up sq))
    (back (square-back sq))))

(defun next-from-position (dir from pos)
  (ecase dir
    (down from)
    (across from)
    (up pos)
    (back pos)))

(defun previous-position (dir sq)
  (next-position (reverse-direction dir) sq))

(defun find-play-column (play)
  (loop for i below (length play)
       do (if (alpha-char-p (char play i))
	      (return i))))

(defun find-play-row (play)
  (loop for i below (length play)
       do (if (digit-char-p (char play i))
	      (return i))))

(defun play-direction (play)
  (let ((c (find-play-column play))
	(r (find-play-row play)))
    (if (< c r) 'down 'across)))

(defun play-column (play)
  (char play (find-play-column play)))

(defun play-row (play)
  (parse-integer play :start (find-play-row play) :junk-allowed t))

(defun play-word-start (square dir)
  (let ((prev (previous-position dir square)))
    (if (and prev (square-letter prev))
	(play-word-start prev dir)
	(values square))))

(defun play-word-filter (square word dir)
  "Check that word can be played at square.  Return letters actually played."
  (if word
      (if square
	  (let ((this (square-letter square)))
	    (if this
		(if (equal this (car word))
		    (play-word-filter (next-position dir square) (cdr word) dir)
		    (error "Play letter does not match board."))
		(cons (car word) 
		      (play-word-filter (next-position dir square) (cdr word) dir))))
	  (error "Play extends off the board."))
      (values nil)))

(defun play-crossword (rules square letter dir)
  (let ((start (play-word-start square dir))
	(next (next-position dir square)))
    (if (and (eq start square)
	     (or (null next) (null (square-letter next))))
	(values 0)
	(play-word-each rules start (cons letter nil) dir 0 1 nil))))

(defun play-word-each (rules square word dir score word-bonus do-crossword)
  (if square
      (if (square-letter square)
	  (play-word-each rules (next-position dir square) word dir
			  (+ score (funcall (rules-letter-points rules)
			     (square-letter square)))
			  word-bonus do-crossword)
	  (if (null word)
	      (* score word-bonus)
	      (prog1
		  (+
		   (play-word-each rules (next-position dir square) (cdr word) dir
				   (+ score (* (funcall (rules-letter-points rules) (car word))
					       (square-letter-bonus square)))
				   (* word-bonus (square-word-bonus square))
				   do-crossword)
		   (if do-crossword
		       (play-crossword rules square (car word) (other-direction dir))
		       0))
		(setf (square-letter square) (car word)))))
      (if word
	  (error "Play extends off the board")
	  (* score word-bonus))))

(defun square-play (rules square dir word)
  (let ((letters (play-word-filter square word dir)))
    (+ (play-word-each rules square letters dir 0 1 t)
       (if (eq (length letters) (rules-rack-size rules)) 50 0))))

(defun board-play (rules board play word)
  "Play word (string) at position on board.  Return points scored."
  (square-play rules
   (board-ref board (play-column play) (play-row play))
   (play-direction play)
   (string-to-list word)))

(defun square-empty (square)
  (or (null square) (null (square-letter square))))

(defun cross-okayp (square dir crossplay docross)
  (if (> (length crossplay) 1)
      (error "too many cross plays! ~A" crossplay))
  (or (eq docross 0)
      crossplay
      (and
       (square-empty (next-position (other-direction dir) square))
       (square-empty (previous-position (other-direction dir) square)))))


(defstruct (play (:print-object play-print))
  "Description of a valid play."
  square				; from
  dir					; play direction
  word					; word played (list of characters)
  points				; word points, not including bonus
  cross					; list of cross plays
  bingo					; True if a full rack was used
  word-bonus				; Accumulated word bonus
  leave					; Rack leave
  )

(defun play-score (play)
  "Total points for a play"
  (let ((points (* (play-points play) (play-word-bonus play))))
    (dolist (p (play-cross play))
      (setf points (+ points (play-score p))))
    (if (play-bingo play)
	(setf points (+ points 50)))
    points))

(defun play-add-cross (play crossplay)
  (setf (play-cross play) (cons crossplay (play-cross play))))

(defun play-explain (play stream)
  (format stream "~A ~A" (list-to-string (play-word play)) (play-points play))
  (if (> (play-word-bonus play) 1)
      (format stream " x ~A" (play-word-bonus play)))
  (dolist (p (play-cross play))
    (format stream " + ")
    (play-explain p stream))
  (if (play-bingo play)
      (format stream " + 50")))

(defun play-print (play stream)
  (format stream "~A ~A, "
	  (square-position (play-square play) (play-dir play)) (play-dir play))
  (play-explain play stream)
  (format stream " = ~A"
	  (play-score play))
  (if (play-leave play)
      (format stream ", leave ~A"
	      (list-to-string (play-leave play)))))

(defun plays-walk-blank
    (rules dict cdict from square dir play rack score bonus docross played collect)
  "Find legal plays with a blank (wildcard) at square."
  (dolist (c (rules-letters rules))
    (setf collect
	  (plays-walk-with rules dict cdict from square
			   dir play rack (char-downcase c)
			   score bonus docross played collect)))
  collect)

(defun plays-walk-with
    (rules dict cdict from square dir play rack letter score bonus docross played collect)
  "Find legal plays with letter at square."
  (if (eql letter (blank))
      (plays-walk-blank rules dict cdict from square dir play
			    rack score bonus docross played collect)
      (progn
	(setf cdict (dictionary-get-letter cdict letter))
	(if cdict
	    (let ((thisplays nil) (crossplays nil))
	      (if (> docross 0)
		  (setf crossplays
			(plays-walk-with
			 rules dict dict square square (other-direction dir)
			 nil nil letter 0 1 0 0 nil)))
	      (if (and (eq docross 2) (eq played 0))
		  (setf collect
			(plays-walk-with
			 rules dict dict square square (other-direction dir)
			 nil rack letter 0 1 1 0 collect)))
	      (if (cross-okayp square dir crossplays docross)
		  (setf thisplays
			(plays-walk rules dict cdict
				    (next-from-position dir from square)
				    (next-position dir square) dir
				    (cons letter play)
				    rack
				    (+ score 
				       (* (funcall (rules-letter-points rules) letter)
					  (square-letter-bonus square)))
				    (* bonus (square-word-bonus square))
				    docross
				    (1+ played) nil)))
	      (dolist (p thisplays)
		(if crossplays
		    (setf (play-cross p)
			  (cons (car crossplays) (play-cross p))))
		(setf collect (cons p collect)))))
	collect)))

; This function walks forward (down or across) until it finds the end
; of a word.  Then, it returns to the place it began from and walks
; backward, matching the prefix for the word.
(defun plays-walk
    (rules dict cdict from square dir play rack score bonus docross played collect)
  "Simultaneously walk the board and the dictionary, looking for plays."
  (if (and (dictionary-terminal cdict) (square-empty square) (> played 0))
      (setq collect
	    (if (eq (dictionary-terminal cdict) t)
		(cons (make-play :square from :dir (reverse-direction dir)
				 :word play :points score
				 :bingo (= played (rack-size))
				 :word-bonus bonus
				 :leave rack) collect)
		(plays-walk rules dict (dictionary-terminal cdict) from
				(previous-position dir from)
				(reverse-direction dir) (reverse play)
				rack score bonus docross played collect))))
  (if square
      (if (square-letter square)
	    (progn ; already a letter here
	      (setf cdict (dictionary-get-letter cdict (square-letter square)))
	      (if cdict
		  (setq collect
		    (plays-walk rules dict cdict 
				(next-from-position dir from square)
				(next-position dir square) dir
				(cons (square-letter square) play) rack
				(+ score (funcall (rules-letter-points rules)
						  (square-letter square)))
				bonus docross played collect))))
	    (dolist (letter rack) ; empty square
	      (setq collect (plays-walk-with rules dict cdict from square
					     dir play
					     (remove letter rack :count 1)
					     letter score bonus
					     docross
					     played collect)))))
  collect)

(defun plays-at (rules dict square dir rack &optional collect)
  (format t "; considering ~A ~A~%" square dir)
  (plays-walk rules dict dict square square dir nil rack 0 1 2 0 collect))

(defun plays-each (rules dict square rack collect)
  "If the given square has a letter, try to play through it."
  (if square
      (progn
	(setf collect (plays-each rules dict (square-next square) rack collect))
	(if (or (square-letter square) (square-start square))
	    (let ((left (previous-position 'across square))
		  (above (previous-position 'down square)))
	      (if (square-empty left)
		  (setq collect (plays-at rules dict square 'across rack collect)))
	      (if (square-empty above)
		  (setq collect (plays-at rules dict square 'down rack collect)))))))
  collect)

(defun plays (rules dict board rack)
  "Find all legal plays with the given rules, dictionary, board and rack."
  (plays-each rules dict (upper-left board) rack nil))