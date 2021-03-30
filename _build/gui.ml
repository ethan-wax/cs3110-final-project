open Graphics

let draw_points_4x5 location =
  match location with
  | x, y ->
      (* Drawing points *)
      for i = 0 to 5 do
        for j = 0 to 4 do
          set_color (rgb 97 97 97);
          fill_circle (x + (i * 100)) (y + (j * 100)) 15
        done
      done

let draw_move coordinates =
  set_color (rgb 120 120 120);
  set_line_width 12;
  moveto (List.nth coordinates 0) (List.nth coordinates 1);
  lineto (List.nth coordinates 2) (List.nth coordinates 3)

let draw_box len pos rgb_color =
  set_color
    (rgb (List.nth rgb_color 0) (List.nth rgb_color 1)
       (List.nth rgb_color 2));
  fill_rect (List.nth pos 0) (List.nth pos 1) len len

let draw_board =
  open_graph "";
  resize_window 800 1000;
  set_window_title "Dots and Boxes";
  (* background = Graphics.rgb 217 175 0; *)
  (* Coodinates are x y plane *)
  clear_graph ();

  (* Set up counter labels *)
  moveto 50 50;
  set_text_size 500;
  set_color red;
  draw_string "Red";
  moveto 700 50;
  set_color blue;
  draw_string "Blue";
  draw_points_4x5 (150, 300);
  (* Draw current move *)
  draw_move [ 150; 300; 150; 400 ];
  (* Draws current box *)
  draw_box 100 [ 150; 300 ] [ 255; 0; 0 ]

let open_board = draw_board

(* let draw_counter values = failwith "Unimplemented" *)

(* Quits the game if q is pressed *)
let quit_game =
  let event = wait_next_event [ Key_pressed ] in
  if event.key == 'q' then close_graph () else ()
