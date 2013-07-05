; Scrabble word dictionary
;
; Generally, words are lists of characters
;
; The dictionary is actually a dictionary of all word suffixes.  That
; is, the word "busy" would include "busy", "usy", "sy" and "y".  For
; each suffix, there is a "prefix dictionary", or pdictionary, of the
; prefix for that suffix, in reverse order.
;
; For example, a dictionary with the word "spot" and "rot" would
; include the suffix "ot", the dictionary node for 't' will have a
; terminal member that is itself a dictionary containing "sp" and "r".
; 
; Because the dictionary contains all possible suffixes for a word,
; searching for a matching word through a square can include words
; that don't begin at that square.  This makes finding legal plays
; much faster since we only have to consider legal play squares.
;
; The tradeoff is the upfront cost to build the dictionary and the
; memory required.

(defstruct dictionary
  (terminal nil)		    ; If this is the end of a word,
				    ; dictionary of prefixes. (or t if
				    ; this is the prefix dictionary)
  (letters nil))		    ; alist of letter, dictionary

(defun dictionary-get-letter (dict letter)
  "Return sub-dictionary associated with letter"
  (let ((leaf (assoc (char-upcase letter) (dictionary-letters dict))))
    (if leaf (cdr leaf) nil)))

(defun dictionary-get-leaf (dict word)
  (dictionary-get-letter dict (car word)))

(defun dictionary-add-leaf (dict word)
  (let ((leaf (dictionary-get-leaf dict word)))
    (if (null leaf)
	(progn
	  (setf leaf (make-dictionary))
	  (setf (dictionary-letters dict)
		(acons (car word) leaf (dictionary-letters dict)))))
    (values leaf)))

(defun dictionary-get-tail (dict word)
  (cond
    (word (dictionary-get-tail (dictionary-add-leaf dict word) (cdr word)))
    (t dict)))

(defun dictionary-add (dict word)
  (setf (dictionary-terminal (dictionary-get-tail dict word)) t))

(defun pdictionary-add-each (dict word rest)
  (if word
      (let ((tail (dictionary-get-tail dict word)))
	(if (null (dictionary-terminal tail))
	    (setf (dictionary-terminal tail) (make-dictionary)))
	(dictionary-add (dictionary-terminal tail) rest)
	(pdictionary-add-each dict (cdr word) (cons (car word) rest)))))

(defun pdictionary-add (dict word)
  (pdictionary-add-each dict word nil))

(defun dictionary-each-with (dict each thisword)
  (if (dictionary-terminal dict)
      (funcall each dict thisword))
  (dolist (leaf (dictionary-letters dict))
    (vector-push-extend (car leaf) thisword)
    (dictionary-each-with (cdr leaf) each thisword)
    (vector-pop thisword)))

(defun dictionary-each (dict each)
  (dictionary-each-with dict
			(function (lambda (leaf word)
			  (declare (ignore leaf))
			  (funcall each word)))
			(make-fill-string)))

(defun pdictionary-prefix-each (dict suffix each)
  (dictionary-each-with dict
			(function (lambda (leaf prefix)
			  (declare (ignore leaf))
			  (funcall each (reverse prefix) suffix)))
			(make-fill-string)))

(defun pdictionary-each (dict each)
  (dictionary-each-with dict
			(function (lambda (leaf suffix)
			  (pdictionary-prefix-each
			   (dictionary-terminal leaf) suffix each)))
			(make-fill-string)))

(defun dictionary-dump (dict)
  (dictionary-each dict (function (lambda (word) (write-line word)))))
  
(defun dictionary-update (dict file)
  (with-open-file (in file)
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (dictionary-add dict (string-to-list line)))))

(defun pdictionary-update (dict file)
  (with-open-file (in file)
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (pdictionary-add dict (string-to-list line)))))

(defun dictionary-lookup-prefix (dict word)
  (cond
    ((null dict) nil)
    ((null word) dict)
    (t (dictionary-lookup-prefix (dictionary-get-leaf dict word) (cdr word)))))

(defun dictionary-lookup (dict word)
  (let ((leaf (dictionary-lookup-prefix dict word)))
    (if (and leaf (dictionary-terminal leaf)) leaf nil)))

(defun load-dictionary (filename)
  (let ((dict (make-dictionary)))
    (dictionary-update dict filename)
    (values dict)))

(defun load-pdictionary (filename)
  (let ((dict (make-dictionary)))
    (pdictionary-update dict filename)
    (values dict)))

(defun lookup (argv)
  (let ((dict (load-dictionary "./wordlist")))
    (dolist (word (cdr argv))
      (let ((leaf (dictionary-lookup-prefix dict (string-to-list word))))
	(if leaf
	    (dictionary-each leaf (function (lambda (suffix) (format t "~A~A~%" word suffix)))))))))

(defun plookup (argv)
  (let ((dict (make-dictionary)))
    (pdictionary-update dict "./shortwords")
    (dolist (word (cdr argv))
      (let ((leaf (dictionary-lookup-prefix dict (string-to-list word))))
	(if leaf
	    (pdictionary-each leaf (function (lambda (prefix suffix) (format t "~A~A~A~%" prefix word suffix)))))))))
