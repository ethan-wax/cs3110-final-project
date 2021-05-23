(** The abstract type of a board. *)
type t

type color =
  | Blank
  | Red
  | Blue

(** [make_board size] initializes a new board with sizes indicated by
    [size]. Requires: Both values of [size] must be greater or equal to
    1. *)
val make_board : int * int -> t

(** [update_board points player board] is [board] with a new connection
    between [points] owned by [player]. Requires: There must be a valid
    path between the two points. *)
val update_board : (int * int) * (int * int) -> color -> t -> t

(** [get_branch points board] is the color of the connection between
    [points] or blank if no connection exists. Requires: There must be a
    valid path between the two points *)
val get_branch : (int * int) * (int * int) -> t -> color

(** [branch_filled points board] have to write the spec. *)
val branch_filled : (int * int) * (int * int) -> t -> bool

(** [dimensions board] are the dimensions of board, i.e. if the board
    was 4x5 then dimension board would return (4x5). Requires: [board]
    has nonzero dimensions. *)
val dimensions : t -> int * int

(** [score board] is a tuple with the scores of the players. *)
val score : t -> int * int

(** [end_game board] is whether or not the game has been finished. *)
val end_game : t -> bool

(** [last_filled board] is the boxes that were last filled in the
    previous move if any. *)
val last_filled : t -> (int * int) list

(** [sides_matrix board] is a matrix representing the filled sides of
    the board. The entry at [ij] is the number of sides that the box at
    [ij] has filled on the board. *)
val sides_matrix : t -> int array array
