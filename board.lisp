; Functions and definitions related to the game board

(defun first-column () "Columns are numbered from a" #\a)
(defun first-row () "Rows are numbered from 1" 1)

(defstruct (square (:print-object print-square-simple))
  (start nil)			    ; True if this square is a starting point
  (letter nil)			    ; Letter at this square
  (letter-bonus 1)		    ; Letter bonus multiplier for this square
  (word-bonus 1)		    ; Word bonus multiplier for this square
  (column nil)
  (row nil)
  (across nil)
  (down nil)
  (back nil)
  (up nil)
  (next nil))

(defun square-bonus-char (square)
  (case (square-word-bonus square)
    (3 #\*)
    (2 #\+)
    (1 (case (square-letter-bonus square)
	 (3 #\=)
	 (2 #\-)
	 (1 #\space)))))

(defun square-letter-char (square)
  (if (square-letter square)
      (square-letter square)
      (square-bonus-char square)))

(defun print-square (sq stream)
  (format stream "~A"
	  (square-letter-char sq)))

(defun make-board-helper (s)
  (case s
    (--- (make-square))
    (3xW (make-square :word-bonus 3))
    (2xW (make-square :word-bonus 2))
    (3xL (make-square :letter-bonus 3))
    (2xL (make-square :letter-bonus 2))
    (2*W (make-square :start t :word-bonus 2))
    (-*- (make-square :start t))))

(defun make-board-row (r)
  (map 'list #'make-board-helper r))

(defun board-connect (array)
  "Given an array of squares, connect links."
  (let ((prev nil))
    (loop for i from 0 below (array-dimension array 0) do
	 (loop for j from 0 below (array-dimension array 1) do
	      (let ((square (aref array i j)))
		(setf (square-column square) (index-col i))
		(setf (square-row square) (index-row j))
		(if prev
		    (setf (square-next prev) square))
		(setf prev square)
		(if (> i 0)
		    (setf (square-back square) (aref array (1- i) j)))
		(if (> j 0)
		    (setf (square-up square) (aref array i (1- j))))
		(if (< i (1- (array-dimension array 0)))
		    (setf (square-across square) (aref array (1+ i) j)))
		(if (< j (1- (array-dimension array 1)))
		    (setf (square-down square) (aref array i (1+ j))))))))
  (values array))

(defstruct (board (:print-function print-board))
  array)

(defun index-row (idx)
  (+ idx (first-row)))

(defun index-col (idx)
  (code-char (+ (char-code (first-column)) idx)))

(defun row-index (row)
  (- row (first-row)))

(defun col-index (col)
  (- (char-code col) (char-code (first-column))))

(defun print-board-row (stream square depth)
  (progn
    (if (null (square-back square))
	(format stream "~vD" (+ depth 2) (square-row square)))
    (format stream " ~A " (square-letter-char square))
    (if (null (square-across square))
	(format stream "~%")
	(print-board-row stream (square-across square) depth))))

(defun print-board-rows (stream square depth)
  (if square
      (progn
	(print-board-row stream square depth)
	(print-board-rows stream (square-down square) depth))))

(defun print-board-columns (stream square)
  (if square
      (progn
	(format stream " ~A " (square-column square))
	(print-board-columns stream (square-across square)))))

(defun print-board (board stream depth)
  (progn
    (format stream "  ")
    (print-board-columns stream (upper-left board))
    (format stream "~%")
    (print-board-rows stream (upper-left board) depth)))
    
(defun board-ref (board col row)
  (aref (board-array board) (col-index col) (row-index row)))

(defun upper-left (board)
  (board-ref board (first-column) (first-row)))

(defun square-position (square &optional (dir 'across))
  (ecase dir
    (down (format nil "~A~A" (square-column square) (square-row square)))
    (across (format nil "~A~A" (square-row square) (square-column square)))))

(defun print-square-simple (sq stream)
  (write-string (square-position sq) stream))

