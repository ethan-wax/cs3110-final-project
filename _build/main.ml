open Board
open Command
open State
open Player

let init_board = Board.make_board (5, 5)

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

(* Need to still call end game variable *)
let rec loop_game board player input =
  if Board.end_game board then (
    print_string "\n The game is over! GGWP\n";
    exit 0)
  else
    let playername = Player.name player in
    print_endline
      ("Hello, " ^ playername ^ "," ^ "\nWhere do you want to go?\n");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | player_move -> parse_input board player player_move

and parse_input board player move =
  match Command.parse move board with
  | Legal t ->
      if Player.name player = "Player 1" then
        loop_game (State.go board player1 t) player2 ""
      else loop_game (State.go board player2 t) player1 ""
  | Illegal ->
      print_string
        ("\n" ^ Player.name player
       ^ ", your move was invalid. Try again!\n");
      loop_game board player move

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Dots and Boxes!.\n";
  print_string "> ";
  loop_game init_board player1 ""

(* Execute the game engine. *)
let () = main ()
