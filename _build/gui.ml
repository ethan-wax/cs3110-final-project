open Graphics
open Command
open Board
open State
open Player

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

let draw_points_mxn location m n =
  match location with
  | x, y ->
      (* Drawing points *)
      for i = 0 to n do
        for j = 0 to m do
          set_color (rgb 97 97 97);
          fill_circle (x + (i * 100)) (y + (j * 100)) 15
        done
      done

let draw_row_labels rows =
  for i = 0 to rows do
    set_color black;
    moveto 92 (793 - (i * 100));
    draw_string (string_of_int i)
  done

let draw_col_labels cols =
  for i = 0 to cols do
    set_color black;
    moveto (146 + (i * 100)) 850;
    draw_string (string_of_int i)
  done

let draw_move coordinates =
  set_color (rgb 120 120 120);
  set_line_width 12;
  moveto (List.nth coordinates 0) (List.nth coordinates 1);
  lineto (List.nth coordinates 2) (List.nth coordinates 3)

let draw_box len pos rgb_col =
  set_color
    (rgb (List.nth rgb_col 0) (List.nth rgb_col 1) (List.nth rgb_col 2));
  fill_rect (List.nth pos 0) (List.nth pos 1) len len

let draw_counter loc count =
  match loc with
  | x, y ->
      moveto x y;
      draw_string (string_of_int count)

let draw_counters board =
  moveto 50 50;
  set_color red;
  draw_string "Red: ";
  (* Set up counter label Blue *)
  moveto 700 50;
  set_color blue;
  draw_string "Blue: ";
  let scores = Board.score board in
  match scores with
  | p1, p2 ->
      draw_counter (90, 50) p1;
      draw_counter (750, 50) p2

let rec int_list_to_string lst s =
  match lst with
  | [] -> s
  | h :: t ->
      if s <> "" then int_list_to_string t (s ^ " " ^ string_of_int h)
      else int_list_to_string t (s ^ string_of_int h)

let rec char_list_to_string lst s =
  match lst with
  | [] -> s
  | h :: t -> char_list_to_string t (s ^ Char.escaped h)

let acc = ref [||]

let display_line move =
  match move with
  | [ r1; c1; r2; c2 ] ->
      let x1 = 150 + (100 * c1) in
      let y1 = 800 - (100 * r1) in
      let x2 = 150 + (100 * c2) in
      let y2 = 800 - (100 * r2) in
      draw_move [ x1; y1; x2; y2 ]
  | _ -> failwith "Precondition violated"

let display_current_player player =
  set_color black;
  moveto 275 200;
  draw_string (Player.name player ^ "'s move")

(* - User input as char - Stored as char array - Press enter - Convert
   char array into a string (command_issued) - Goes into
   display_valid_move (parse) - display_valid_move is unit displaying on
   board - Then call player input (mutually recursive) *)
let display_valid_move s board player =
  let parsed = parse s board in
  moveto 275 250;
  match parsed with
  | Legal move ->
      display_line move;
      display_current_player player1;
      moveto 275 130;
      draw_string ("Legal move: " ^ int_list_to_string move "")
  | Illegal ->
      display_line [ 0; 0; 0; 0 ];
      moveto 275 130;
      draw_string "This is an illegal move!"

let rec player_input () board player =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' -> close_graph ()
  | '\r' -> command_issued acc board player
  | key -> char_input acc key board player

and command_issued acc board player =
  (* //convert array into a string pass that string into
     display_valid_move make the array empty player_input()*)
  let a = Array.to_list !acc in
  let str = char_list_to_string a "" in
  set_color red;
  fill_rect 250 100 300 75;
  display_valid_move str board player;
  acc := [||];
  player_input () board player

and char_input acc key board player =
  acc := Array.append !acc [| key |];
  moveto 275 150;
  for i = 0 to Array.length !acc - 1 do
    draw_char (Array.get !acc i)
  done;
  player_input () board player

let draw_board =
  let default_board = make_board (5, 5) in
  open_graph "";
  resize_window 800 1000;
  set_window_title "Dots and Boxes";
  clear_graph ();
  (* Set up Game Title *)
  moveto 350 900;
  set_color black;
  draw_string "Dots and Boxes!";
  draw_points_mxn (150, 300) 5 5;
  set_color black;
  set_line_width 3;
  draw_rect 250 100 300 75;
  draw_counters default_board;
  draw_row_labels 5;
  draw_col_labels 5;
  display_current_player player1;
  player_input () default_board player1

let open_board = draw_board
