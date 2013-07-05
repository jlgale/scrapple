(dolist (file '("utils.lisp"
		"letters.lisp"
		"board.lisp"
		"rules.lisp"
		"dictionary.lisp"
		"play.lisp"
		;"game.lisp"
		"client.lisp"
		"english.lisp"
		"scrabble.lisp"
		"wwf.lisp"
		))
  (compile-file file)
  (load file))
