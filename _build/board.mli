type t

type color

(** [make_board size] initializes a new board with sizes indicated by
    [size] *)
val make_board : int * int -> t

(** [update_board points player board] is [board] with a new connection
    between [points] owned by [player]*)
val update_board : 'a -> 'b -> t -> t

(** [get_branch points] is the color of the connection between [points]
    or blank if no connection exists*)
val get_branch : 'a -> color
