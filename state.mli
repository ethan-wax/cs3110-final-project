open Board
open Command
open Player

type t

type move = Result

(** [go board player move] is the board that is created after [player]
    makes [move] on [board] *)
val go : Board.t -> Player.t -> move -> Board.t
