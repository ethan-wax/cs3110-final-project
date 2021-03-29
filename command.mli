type result =
  | Legal of (int * int) * (int * int)
  | Illegal

(** a move has the form (1,1) to (2,1). [parse str] takes a player's
    string input and decides if it is legal or not. If it is legal, it
    returns a type Legal with a nested 2 tuple. i.e. parse (2,2) to
    (3,2) would return ((2,2),(3,2)) if it is illegal, it would return
    Illegal.*)
val parse : string -> result
