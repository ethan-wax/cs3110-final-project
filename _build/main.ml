open Board
open Command
open State
open Player

let init_board = Board.make_board (5, 5)

let player1 = create_player "Player 1: Tim" "Red"

let player2 = create_player "Player 2: Evan" "Blue"

(* When the game ends, displays who won and each players respective
   score*)
let display_endgame board =
  match Board.score board with
  | p1score, p2score ->
      if p1score > p2score then
        print_string
          ("\n" ^ Player.name player1
         ^ " wins, the game is over! GGWP\n" ^ Player.name player1
         ^ " score: " ^ string_of_int p1score ^ "\n"
         ^ Player.name player2 ^ " score: " ^ string_of_int p2score
         ^ "\n")
      else
        print_string
          ("\n" ^ Player.name player2
         ^ " wins, the game is over! GGWP\n" ^ Player.name player1
         ^ " score: " ^ string_of_int p1score ^ "\n"
         ^ Player.name player2 ^ " score: " ^ string_of_int p2score
         ^ "\n")

let rec loop_game board player input =
  if Board.end_game board then display_endgame board
  else
    let playername = Player.name player in
    print_endline (playername ^ "'s" ^ " turn.\n");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | player_move -> parse_input board player player_move

and parse_input board player move =
  if String.trim move = "quit" then exit 0;
  match Command.parse move board with
  | Legal t -> (
      match State.go board player t with
      | Valid bo ->
          if Player.name player = Player.name player1 then
            loop_game bo player2 ""
          else loop_game bo player1 ""
      | Invalid ->
          print_string "Position already occupied, try again.";
          loop_game board player move)
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
