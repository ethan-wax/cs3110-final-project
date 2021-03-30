(** openBoard opens the graph for the user and defines the background
    color and initial dimensions. *)
val open_board : unit

(* Draws the text per red or blue player *)
val draw_board : unit

(* [draw board starting_location] draws a grid of circles on the board
   with the first circle (0,0) centered at location and the rest the
   defined off of that*)
val draw_points_4x5 : int * int -> unit

(* Displays the counter for each player *)
val draw_counter : int list -> unit

(** quit_game closes the game window*)
val quit_game : unit
