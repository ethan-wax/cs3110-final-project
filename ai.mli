(** Easy, Medium, and Hard AI *)
open Board

val empty_sides : Board.t -> string list

(** Takes in a board and returns a move. Easy AI randomly picks
    available moves and returns the move in the format "r1 c2 r2 c2". *)
val easy : Board.t -> string

(** Takes in a board and returns a medium move. Medium AI prioritizes
    types of moves in this order: Completing boxes, filling in edges,
    random position. Each move is in the format "r1 c2 r2 c2" *)
val medium : Board.t -> string

(** Takes in a board and returns a hard move, in the format "r1 c2 r2
    c2" *)
val hard : Board.t -> string
