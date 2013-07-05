(defstruct rules
  letters        ; list of legal letters (not including blank tiles)
  letter-points  ; function which takes a letter and returns how many points it is worth
  make-board     ; function which constructs an empty game board
  make-bag       ; function which constructs a full game bag
  leave-score    ; function which rates a leave. takes a bag and a leave
  rack-size)
  