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

let make_board size =
  (*handle edge cases*)
  let start_point =
    {
      up = Some Blank;
      down = Some Blank;
      left = Some Blank;
      right = Some Blank;
    }
  in
  Array.make (fst size) (Array.make (snd size) start_point)

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

let get_branch points =
  let point1, point2 = (fst points, snd points) in
  let x1, y1 = (fst point1, snd point1) in
  let x2, y2 = (fst point2, snd point2) in
  failwith "TODO"

let dimensions board = failwith "TODO"
