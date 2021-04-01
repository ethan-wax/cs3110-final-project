open Graphics

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

let acc = ref [||]

let rec player_input () =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' -> close_graph ()
  | key -> char_input acc key

and char_input acc key =
  acc := Array.append !acc [| key |];
  moveto 275 150;
  for i = 0 to Array.length !acc - 1 do
    draw_char (Array.get !acc i)
  done;
  player_input ()

let draw_board =
  open_graph "";
  resize_window 800 1000;
  set_window_title "Dots and Boxes";
  (* background = Graphics.rgb 217 175 0; *)
  (* Coodinates are x y plane *)
  clear_graph ();
  (* Set up counter label Red *)
  moveto 50 50;
  set_text_size 500;
  set_color red;
  draw_string "Red: ";
  (* Set up counter label Blue *)
  moveto 700 50;
  set_color blue;
  draw_string "Blue: ";
  (* Set up Game Title *)
  moveto 350 900;
  set_color black;
  draw_string "Dots and Boxes!";
  draw_points_mxn (150, 300) 4 5;
  (* Draw current move *)
  draw_move [ 150; 300; 150; 400 ];
  (* Draws current box *)
  draw_box 100 [ 150; 300 ] [ 255; 0; 0 ];
  (* Draws counter red *)
  set_color black;
  draw_counter (90, 50) 2;
  (* Draws counter blue *)
  set_color black;
  draw_counter (750, 50) 3;
  set_line_width 3;
  draw_rect 250 100 300 75;
  player_input ()

let open_board = draw_board
