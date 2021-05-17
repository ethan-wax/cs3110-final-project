open Board
open Command
open State
open Player
open Ai

let init_board = Board.make_board (5, 5)

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

let easy_bot = create_player "Easy Bot" "Blue"

let medium_bot = create_player "Medium Bot" "Blue"

let hard_bot = create_player "Hard Bot" "Blue"

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

let rec loop_mode mode =
  ANSITerminal.print_string [ ANSITerminal.red ] mode;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
    \ Type the mode you want to play: Easy | Medium | Hard | Multiplayer\n";
  match read_line () with
  | exception End_of_file -> ()
  | player_input ->
      if player_input = "Multiplayer" then
        loop_game init_board player1 "" "Multiplayer"
      else if player_input = "Easy" then
        loop_game init_board player1 "" "Easy"
      else if player_input = "Medium" then
        loop_game init_board player1 "" "Medium"
      else if player_input = "Hard" then
        loop_game init_board player1 "" "Hard"
      else loop_mode "\n Error: Not a valid mode!\n"

and loop_game board player input mode =
  if Board.end_game board then display_endgame board
  else
    let playername = Player.name player in
    print_endline (playername ^ "'s" ^ " turn.\n");

    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | player_move ->
        if mode = "Easy" || mode = "Medium" || mode = "Hard" then
          parse_ai_input board player player_move mode
        else parse_input board player player_move

(* Check the level of the bot, get the move from the bot, display it to
   the user, and make a new board based on the move.*)
and parse_ai_input board player move level =
  if String.trim move = "quit" then exit 0;
  if String.trim move = "score" then score board player level
  else
    match Command.parse move board with
    | Legal t -> (
        match State.go board player t with
        | Valid (bo, li) ->
            (* If player did not get a box, switches to other player's
               turn *)
            if List.length li = 0 then failwith "unip"
            else loop_game bo player "" level
        | Invalid ->
            print_string "Position already occupied, try again. \n";
            loop_game board player move level)
    | Illegal ->
        print_string
          ("\n" ^ Player.name player
         ^ ", your move was invalid. Try again!\n");
        loop_game board player move level

and parse_input board player move =
  if String.trim move = "quit" then exit 0;
  if String.trim move = "score" then score board player "Mult"
  else
    match Command.parse move board with
    | Legal t -> (
        match State.go board player t with
        | Valid (bo, li) ->
            (* If player did not get a box, switches to other player's
               turn *)
            if List.length li = 0 then
              if Player.name player = Player.name player1 then
                loop_game bo player2 "" "Mult"
              else loop_game bo player1 "" "Mult"
            else loop_game bo player "" "Mult"
        | Invalid ->
            print_string "Position already occupied, try again. \n";
            loop_game board player move "Mult")
    | Illegal ->
        print_string
          ("\n" ^ Player.name player
         ^ ", your move was invalid. Try again!\n");
        loop_game board player move "Mult"

and score board player mode =
  print_string
    ("\n" ^ "-------------------------\n" ^ Player.name player1
   ^ " score: "
    ^ string_of_int (fst (Board.score board))
    ^ "\n" ^ Player.name player2 ^ " score: "
    ^ string_of_int (snd (Board.score board))
    ^ "\n-------------------------" ^ "\n");
  loop_game board player "" mode

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to Dots and Boxes!. Type 'quit' to exit the game and \n\
    \    'score' to view the score.\n";
  print_string "> ";
  loop_mode ""

(* Execute the game engine. *)
let () = main ()
