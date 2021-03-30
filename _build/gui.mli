(** openBoard opens the graph for the user and defines the background
    color and initial dimensions. *)
val open_board : unit

(* Draws the text per red or blue player *)
val draw_board : unit

(** quit_game closes the game window*)
val quit_game : unit

(* [draw_move coordinates] takes a list of coordinates which is are
   starting and ending points Ex: 1 3 4 2 represents (1,3) (4,2).
   Precondition: coordinates is a valid list of two coordinates *)
val draw_move : int list -> unit

(* [draw_box len pos rgbcol] draws a box on the screen with side length
   len, positioned with its bottom left corner at pos. The box will be
   filled in with color represented by rgbcol, which is an list of
   length 3 where each index represents r, g, b in the rgb color model. *)
val draw_box : int -> int list -> int list -> unit

(* [draw board starting_location] draws a grid of circles on the board
   with the first circle (0,0) centered at location and the rest the
   defined off of that*)
val draw_points_4x5 : int * int -> unit

(* (* Displays the counter for each player *) val draw_counter : int
   list -> unit *)
