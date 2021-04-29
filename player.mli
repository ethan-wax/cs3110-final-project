open Board

type t

(** [name player] is the name associated with player. *)
val name : t -> string

(** [color player] is the color associated with player. *)
val color : t -> Board.color

(** [create_player name color] creates a new player named [name] and
    colored [color]*)
val create_player : string -> string -> t
