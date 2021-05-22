open Board
open Command
open State
open Player
open Ai

let init_board = Board.make_board (5, 5)

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

let bot = create_player "Easy Bot" "Blue"

(* The number of moves player 1 has made *)
let p1moves = ref 0

(* The number of moves player 2 has made *)
let p2moves = ref 0

(* The number of moves the bot has made *)
let botmoves = ref 0

let inc_move player_moves = incr player_moves

(* When the game ends, displays who won and each players respective
   score *)
let display_endgame board mode =
  let p2name =
    if mode = "Mult" then Player.name player2
    else if mode = "Easy" then "Easy Bot"
    else if mode = "Medium" then "Medium Bot"
    else "Hard Bot"
  in
  match Board.score board with
  | p1score, p2score ->
      if p1score > p2score then
        print_string
          ("\n" ^ "------------------------------\n"
         ^ Player.name player1 ^ " wins, the game is over! GGWP\n"
         ^ Player.name player1 ^ " score: " ^ string_of_int p1score
         ^ "\n" ^ p2name ^ " score: " ^ string_of_int p2score ^ "\n"
         ^ "------------------------------\n")
      else
        print_string
          ("\n" ^ "------------------------------\n" ^ p2name
         ^ " wins, the game is over! GGWP\n" ^ Player.name player1
         ^ " score: " ^ string_of_int p1score ^ "\n" ^ p2name
         ^ " score: " ^ string_of_int p2score ^ "\n"
         ^ "------------------------------\n")

let rec loop_mode mode =
  ANSITerminal.print_string [ ANSITerminal.red ] mode;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
    \ Type the mode you want to play: Easy | Medium | Hard | Multiplayer\n";
  match read_line () with
  | exception End_of_file -> ()
  | player_input ->
      let player_input = String.trim player_input in
      if player_input = "Multiplayer" then
        loop_game init_board player1 "" "Mult"
      else if player_input = "Easy" then
        loop_game init_board player1 "" "Easy"
      else if player_input = "Medium" then
        loop_game init_board player1 "" "Medium"
      else if player_input = "Hard" then
        loop_game init_board player1 "" "Hard"
      else loop_mode "\n Error: Not a valid mode!\n"

and loop_game board player input mode =
  if Board.end_game board then display_endgame board mode
  else
    let playername = Player.name player in
    print_endline (playername ^ "'s" ^ " turn.\n");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | player_move -> parse_input board player player_move mode

(* Based on the players input, either quits out of the game , shows the
   player stats, shows the board, shows the score, or makes a move.*)
and parse_input board player player_move mode =
  if String.trim player_move = "quit" then exit 0;
  if String.trim player_move = "stats" then
    player_stats board player mode;
  if String.trim player_move = "board" then
    print_board board player mode;
  if String.trim player_move = "score" then score board player mode
  else if mode = "Easy" || mode = "Medium" || mode = "Hard" then
    parse_ai_input board player player_move mode
  else parse_mult_input board player player_move

(* Check the level of the bot, get the move from the bot, display it to
   the user, and make a new board based on the move.*)
and parse_ai_input board player move level =
  match Command.parse move board with
  | Legal t -> (
      match State.go board player t with
      | Valid (bo, li) ->
          (* If player did not get a box, switches to other player's
             turn *)
          inc_move p1moves;
          if List.length li = 0 then bot_move board level
          else loop_game bo player "" level
      | Invalid ->
          print_string "Position already occupied, try again. \n";
          loop_game board player move level)
  | Illegal ->
      print_string
        ("\n" ^ Player.name player
       ^ ", your move was invalid. Try again!\n");
      loop_game board player move level

and bot_move board level =
  let move =
    if level = "Easy" then Ai.easy board
    else if level = "Medium" then Ai.medium board
    else Ai.hard board
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\nBot move: " ^ move ^ "\n");
  match Command.parse move board with
  | Legal t -> (
      match State.go board bot t with
      | Valid (bo, li) ->
          (* If player did not get a box, switches to other player's
             turn *)
          inc_move botmoves;
          if List.length li = 0 then loop_game bo player1 "" level
          else if Board.end_game bo then display_endgame bo level
          else bot_move bo level
      | Invalid ->
          failwith "impossible, bot will always make a valid move")
  | Illegal -> failwith "impossible, bot will always make a legal move"

and parse_mult_input board player move =
  match Command.parse move board with
  | Legal t -> (
      match State.go board player t with
      | Valid (bo, li) ->
          (* If player did not get a box, switches to other player's
             turn *)
          if Player.name player = Player.name player1 then
            inc_move p1moves
          else inc_move p2moves;
          if List.length li = 0 then
            if Player.name player = Player.name player1 then
              loop_game bo player2 "" "Mult"
            else loop_game bo player1 "" "Mult"
          else
            (*(if Player.name = Player.name player1 then inc_move
              p1moves else inc_move p2moves) *)
            loop_game bo player "" "Mult"
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

and player_stats board player mode =
  print_string
    ("\n" ^ "-------------------------\n" ^ Player.name player1
   ^ " moves: " ^ string_of_int !p1moves ^ "\n");
  if mode = "Mult" then
    print_string
      (Player.name player2 ^ " moves: " ^ string_of_int !p2moves
     ^ "\n-------------------------" ^ "\n")
  else
    print_string
      (Player.name bot ^ " moves: " ^ string_of_int !botmoves
     ^ "\n-------------------------" ^ "\n");
  loop_game board player "" mode

and print_board board player mode =
  let row1 = row_colors board 0 0 5 in
  let row2 = row_colors board 0 1 5 in
  let row3 = row_colors board 0 2 5 in
  let row4 = row_colors board 0 3 5 in
  let row5 = row_colors board 0 4 5 in
  let row6 = row_colors board 0 5 5 in
  let col1 = col_colors board 0 0 5 in
  let col2 = col_colors board 1 0 5 in
  let col3 = col_colors board 2 0 5 in
  let col4 = col_colors board 3 0 5 in
  let col5 = col_colors board 4 0 5 in
  let col6 = col_colors board 5 0 5 in
  let rows = [ row1; row2; row3; row4; row5; row6 ] in
  let cols = [ col1; col2; col3; col4; col5; col6 ] in
  print_rows rows;
  loop_game board player "" mode

(* Row_colors returns a list with just the colors in each row. I is the
   current index it is on, and j is what it is going to*)
and row_colors board x y j =
  let c = get_branch ((x, y), (x + 1, y)) board in
  if j - x = 1 then [ string_of_color c ]
  else string_of_color c :: row_colors board (x + 1) y j

and col_colors board x y j =
  let c = get_branch ((x, y), (x, y + 1)) board in
  if j - y = 1 then [ string_of_color c ]
  else string_of_color c :: col_colors board x (y + 1) j

and string_of_color c =
  match c with Red -> "R" | Blue -> "B" | Blank -> "N"

and print_rows rows =
  let print_row r =
    List.iter print_string r;
    print_string "\n-------------------------\n"
  in
  List.iter print_row rows

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to Dots and Boxes!. Type 'quit' to exit the game, type \
     'stats' to view the game statistics, and \n\
    \    'score' to view the score.\n";
  print_string "> ";
  loop_mode ""

(* Execute the game engine. *)
let () = main ()
