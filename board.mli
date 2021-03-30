type t

type color

(** [make_board size] initializes a new board with sizes indicated by
    [size]. Requires: Both values of [size] must be greater or equal to
    1. *)
val make_board : int * int -> t

(** [update_board points player board] is [board] with a new connection
    between [points] owned by [player]. Requires: There must be a valid
    path between the two points. *)
val update_board : (int * int) * (int * int) -> color -> t -> unit

(** [get_branch points board] is the color of the connection between
    [points] or blank if no connection exists. Requires: There must be a
    valid path between the two points *)
val get_branch : (int * int) * (int * int) -> t -> color

(** [dimensions board] are the dimensions of board, i.e. if the board
    was 4x5 then dimension board would return (4x5). Requires: [board]
    has nonzero dimensions. *)
val dimensions : t -> int * int
