(defun string-to-list (s)
  "Convert a string to a list of characters."
  (reduce #'cons s :from-end t :initial-value nil))

(defun list-to-string (l)
  "Convert a list of characters into a string."
  (make-array (length l) :element-type 'character
	      :adjustable nil :initial-contents l))

(defun make-fill-string ()
    (make-array 0 :element-type 'character
     :fill-pointer 0 :adjustable t))

(defun repeat (n a rest)
  "Prepend a to rest n times, returning the new list."
  (if (= n 0)
      rest
      (repeat (1- n) a (cons a rest))))

(defun uncompress (list)
  "Given a 'compressed' alist, expand to a normal list."
  (if (null list)
      nil
      (repeat (cdr (car list)) (car (car list)) (uncompress (cdr list)))))

(defun compress-each (a n rest)
  (if (null rest)
      (cons (cons a n) nil)
      (if (eql a (car rest))
	  (compress-each a (+ n 1) (cdr rest))
	  (cons (cons a n) (if (cdr rest)
			       (compress-each (car rest) 1 (cdr rest))
			       nil)))))

(defun compress (list)
  "Given a sorted list, return a 'compressed' alist of (value . count)."
  (if (null list)
      nil
      (compress-each (car list) 1 (cdr list))))
      
(declaim (ftype (function (function &rest t) function) curry)
	 (inline curry))

(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))