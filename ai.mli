open Board

(* Takes in a board and returns an easy move, in the format "r1 c2 r2
   c2" *)
val easy : Board.t -> string

(* Takes in a board and returns a medium move, in the format "r1 c2 r2
   c2" *)
val medium : Board.t -> string

(* Takes in a board and returns a hard move, in the format "r1 c2 r2 c2" *)
val hard : Board.t -> string