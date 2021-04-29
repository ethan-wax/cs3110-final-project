type color =
  | Blank
  | Red
  | Blue

type point = {
  up : color option;
  down : color option;
  left : color option;
  right : color option;
}

type t = {
  board : point array array;
  score : int * int;
  full : bool;
  dim : int * int;
}

type direc =
  | Left
  | Right
  | Up
  | Down

exception Out_of_Board

let make_board size =
  assert (fst size > 0);
  assert (snd size > 0);
  let rows = fst size + 1 in
  let cols = snd size + 1 in
  let handle_edges arr =
    Array.iter (fun x -> x.(0) <- { (x.(0)) with up = None }) arr;
    Array.iter
      (fun x -> x.(cols - 1) <- { (x.(cols - 1)) with down = None })
      arr;
    arr.(0) <- Array.map (fun x -> { x with left = None }) arr.(0);
    arr.(rows - 1) <-
      Array.map (fun x -> { x with right = None }) arr.(rows - 1)
  in
  let a =
    Array.make rows
      (Array.make cols
         {
           up = Some Blank;
           down = Some Blank;
           left = Some Blank;
           right = Some Blank;
         })
  in
  handle_edges a;
  {
    board = a;
    score = (0, 0);
    full = false;
    dim = (rows - 1, cols - 1);
  }

let direction x1 x2 y1 y2 =
  if x2 - x1 = 1 then Right
  else if x1 - x2 = 1 then Left
  else if y2 - y1 = 1 then Down
  else Up

let get_branch points board =
  let brd = board.board in
  let point1, point2 = (fst points, snd points) in
  let x1, y1 = (fst point1, snd point1) in
  let x2, y2 = (fst point2, snd point2) in
  let d = direction x1 x2 y1 y2 in
  let color_opt =
    match d with
    | Left -> brd.(x1).(y1).left
    | Right -> brd.(x1).(y1).right
    | Up -> brd.(x1).(y1).up
    | Down -> brd.(x1).(y1).down
  in
  match color_opt with Some c -> c | None -> raise Out_of_Board

let branch_filled points board =
  match get_branch points board with
  | Red | Blue -> true
  | Blank -> false

let update_board points color board =
  let brd = board.board in
  let point1, point2 = (fst points, snd points) in
  let x1, y1 = (fst point1, snd point1) in
  let x2, y2 = (fst point2, snd point2) in
  let p1_direc = direction x1 x2 y1 y2 in
  let p2_direc =
    match p1_direc with
    | Left -> Right
    | Right -> Left
    | Up -> Down
    | Down -> Up
  in
  let update_point x y = function
    | Left -> brd.(x).(y) <- { (brd.(x).(y)) with left = Some color }
    | Right -> brd.(x).(y) <- { (brd.(x).(y)) with right = Some color }
    | Down -> brd.(x).(y) <- { (brd.(x).(y)) with down = Some color }
    | Up -> brd.(x).(y) <- { (brd.(x).(y)) with up = Some color }
  in
  update_point x1 y1 p1_direc;
  update_point x2 y2 p2_direc

(* We're taking 1 off here becuase we are looking for the number of
   boxes, while the length of the array is the number of points *)
let dimensions board = board.dim

let score board = board.score

let end_game board = board.full
