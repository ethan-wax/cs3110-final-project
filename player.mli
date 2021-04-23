open Board

type t

(** [color player] is the color associated with player. *)
val color : t -> color

(** [name player] is the name associated with player. *)
val name : t -> string

(** [create_player name color] creates a new player named [name] and
    colored [color]*)
val create_player : string -> color -> t
