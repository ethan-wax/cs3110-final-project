open Player

(** open_board opens the graph for the user and defines the background
    color and initial dimensions. *)
val open_board : unit

(** Draws the text per red or blue player *)
val draw_board : int * int -> int * int -> int * int * int * int -> unit

(** [draw_move coordinates] takes a list of coordinates which is are
    starting and ending points Ex: 1 3 4 2 represents (1,3) (4,2).
    Precondition: coordinates is a valid list of two coordinates *)
val draw_move : int list -> unit

(** [draw_box len pos rgb_col] draws a box on the screen with side
    length len, positioned with its bottom left corner at pos. The box
    will be filled in with color represented by rgbcol, which is an list
    of length 3 where each index represents r, g, b in the rgb color
    model. *)
val draw_box : int -> int list -> int list -> unit

(** [draw grid location m n ] draws a grid of circles, with the
    respective amount of m rows and n columns, starting at the top left
    of the board. *)
val draw_grid : int * int -> int -> int -> unit

(** [draw_counter loc count] Displays the score count of a player at a
    loc on the board.*)
val draw_counter : int * int -> int -> unit

(* OUTDATED MLI---------------------- *)

(** [player_input () board] waits for user input and responds depending
    on which key is pressed *)
val player_input : unit -> Board.t -> Player.t -> string -> unit
