open Board

type result =
  | Legal of int list
  | Illegal

(** A move has the form 1 1 2 1. This means the player is drawing a line
    from point (1,1) to point (2,1). [parse str] takes a player's string
    input and decides if it is legal or not. If it is legal, it returns
    a type Legal of int list representing their move. For example, parse
    "2 3 3 3" would return [2 3 3 3]. But parse "banana" return Illegal.*)
val parse : string -> Board.t -> result
