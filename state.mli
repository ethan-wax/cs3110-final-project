open Board
open Command
open Player

type t

type state_result =
  | Valid of Board.t * (int * int) list
  | Invalid

(* move represents a valid move on the board. Precondition: *)
type move = int list

(** [go board player move] is the board that is created after [player]
    makes [move] on [board]. Returns Invalid if the move has already
    been made on the board. *)
val go : Board.t -> Player.t -> move -> state_result
