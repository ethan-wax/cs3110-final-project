open Board
open Command
open State
open Player

type ai =
  | Easy
  | Medium
  | Hard

let init_board = Board.make_board (5, 5)

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

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
  if String.trim move = "score" then score board player
  else
    match Command.parse move board with
    | Legal t -> (
        match State.go board player t with
        | Valid (bo, li) ->
            (* If player did not get a box, switches to other player's
               turn *)
            if List.length li = 0 then
              if Player.name player = Player.name player1 then
                loop_game bo player2 ""
              else loop_game bo player1 ""
            else loop_game bo player ""
        | Invalid ->
            print_string "Position already occupied, try again. \n";
            loop_game board player move)
    | Illegal ->
        print_string
          ("\n" ^ Player.name player
         ^ ", your move was invalid. Try again!\n");
        loop_game board player move

and score board player =
  print_string
    ("\n" ^ "-------------------------\n" ^ Player.name player1
   ^ " score: "
    ^ string_of_int (fst (Board.score board))
    ^ "\n" ^ Player.name player2 ^ " score: "
    ^ string_of_int (snd (Board.score board))
    ^ "\n-------------------------" ^ "\n");

  loop_game board player ""

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to Dots and Boxes!. Type 'quit' to exit the game and \n\
    \    'score' to view the score.\n";
  print_string "> ";
  loop_game init_board player1 ""

(* Execute the game engine. *)
let () = main ()
