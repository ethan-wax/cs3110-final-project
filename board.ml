type color =
  | Blank
  | Red
  | Blue

type point = {
  mutable up : color option;
  mutable down : color option;
  mutable left : color option;
  mutable right : color option;
}

type t = point array array

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
    Array.iter (fun x -> x.left <- None) arr.(0);
    Array.iter (fun x -> x.right <- None) arr.(cols - 1);
    Array.iter (fun x -> x.(0).up <- None) arr;
    Array.iter (fun x -> x.(rows - 1).down <- None) arr
  in
  let start_point =
    {
      up = Some Blank;
      down = Some Blank;
      left = Some Blank;
      right = Some Blank;
    }
  in
  let arr = Array.make rows (Array.make cols start_point) in
  handle_edges arr;
  arr

let direction x1 x2 y1 y2 =
  if x2 - x1 = 1 then Left
  else if x1 - x2 = 1 then Right
  else if y2 - y1 = 1 then Down
  else Up

let update_board points color board =
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
    | Left -> board.(x).(y).left <- color
    | Right -> board.(x).(y).right <- color
    | Down -> board.(x).(y).down <- color
    | Up -> board.(x).(y).up <- color
  in
  update_point x1 y1 p1_direc;
  update_point x2 y2 p2_direc

let get_branch points board =
  let point1, point2 = (fst points, snd points) in
  let x1, y1 = (fst point1, snd point1) in
  let x2, y2 = (fst point2, snd point2) in
  let d = direction x1 x2 y1 y2 in
  let color_opt =
    match d with
    | Left -> board.(x1).(y1).left
    | Right -> board.(x1).(y1).right
    | Up -> board.(x1).(y1).up
    | Down -> board.(x1).(y1).down
  in
  match color_opt with Some c -> c | None -> raise Out_of_Board

(* We're taking 1 off here becuase we are looking for the number of
   boxes, while the length of the array is the number of points *)
let dimensions board =
  assert (Array.length board > 1);
  assert (Array.length board.(0) > 1);
  (Array.length board - 1, Array.length board.(0) - 1)
