SBCL=sbcl

SRC = utils.lisp \
      letters.lisp \
      board.lisp \
      dictionary.lisp \
      play.lisp

.PHONEY: all
all: $(patsubst %.lisp,%.fasl,$(SRC))



%.fasl: %.lisp
	$(SBCL) --non-interactive --eval '(compile-file "$<")'

lookup: dictionary.lisp
	buildapp --output $@ --entry lookup --load $<

plookup: dictionary.lisp
	buildapp --output $@ --entry plookup --load $<

.PHONY: tags clean
tags: TAGS

TAGS: utils.lisp letters.lisp board.lisp play.lisp dictionary.lisp scrabble.lisp
	etags $^

clean:
	$(RM) *.fasl