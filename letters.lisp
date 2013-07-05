; Functions for bags of letters

(defun blank () #\?)

(defun sort-letters (letters)
  (sort (copy-list letters) #'(lambda (a b) (< (char-code a) (char-code b)))))

(defun bag-remove (bag letter)
  (setf (bag-tiles bag) (delete letter (bag-tiles bag) :count 1)))

(defun bag-remove-rack (bag rack)
  (if rack
      (progn
	(bag-remove bag (car rack))
	(bag-remove-rack bag (cdr rack)))))

(defun pick-letter (bag)
  (let ((letter (nth (random (length (bag-tiles bag))) (bag-tiles bag))))
    (bag-remove bag letter)
    letter))

(defun pick-letters (bag n &optional tail)
  (if (or (eq n 0)
	  (null (bag-tiles bag)))
      (values tail)
      (cons (pick-letter bag) (pick-letters bag (- n 1) tail))))

(defun bag-exchange (bag letters &optional tail)
  (if (< (length (bag-tiles bag)) (length letters))
      (error "Too few tiles to exchange.")
      (prog1
	  (pick-letters bag (length letters) tail)
	(setf (bag-tiles bag) (append letters (bag-tiles bag))))))

(defstruct bag
  tiles)
