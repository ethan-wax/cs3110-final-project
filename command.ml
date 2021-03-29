open Board

type result =
  | Legal of (int * int) * (int * int)
  | Illegal

(* Splits the user's input into an array *)
let split_list s = String.split_on_char ' ' s

(** Checks if s can be represented as int*)
let is_int s = try int_of_string s with Failure f -> -1

(** Checks if every member of list can be represented as the zero int or
    a positive int.*)
let valid_list string_list =
  let int_list = List.map is_int string_list in
  let rec parse_list_helper lst =
    match lst with
    | [] -> true
    | h :: t -> if h < 0 then false else parse_list_helper t
  in
  parse_list_helper int_list

(* Assumes that 1 was already added to row and column to account for
   dots vs boxes board size *)
let in_bounds int_list r c =
  if List.nth int_list 0 > r || List.nth int_list 2 > r then false
  else if List.nth int_list 1 > c || List.nth int_list 3 > c then false
  else true

(* ----- Helper functions for valid move ----- *)
let top_left r2 c2 =
  if (r2 = 1 && c2 = 0) || (r2 = 0 && c2 = 1) then true else false

let bottom_right r1 c1 r2 c2 =
  if (c1 - 1 = c2 && r1 = r2) || (c1 = c2 && r1 - 1 = r2) then true
  else false

let bottom_left r1 c1 r2 c2 =
  if (c1 = c2 && r1 - 1 = r2) || (c1 + 1 = c2 && r1 = r2) then true
  else false

let top_right r1 c1 r2 c2 =
  if (c1 - 1 = c2 && r1 = r2) || (c1 = c2 && r1 + 1 = r2) then true
  else false

let top_side c1 r2 c2 =
  if
    (r2 = 0 && c1 - 1 = c2)
    || (r2 = 1 && c1 = c2)
    || (r2 = 0 && c1 + 1 = c2)
  then true
  else false

let bottom_side r1 c1 r2 c2 =
  if
    (r1 = r2 && c1 - 1 = c2)
    || (r1 - 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 + 1 = c2)
  then true
  else false

let left_side r1 r2 c2 =
  if
    (r1 - 1 = r2 && c2 = 0)
    || (r1 = r2 && c2 = 1)
    || (r1 + 1 = r2 && c2 = 0)
  then true
  else false

let right_side r1 c1 r2 c2 =
  if
    (r1 - 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 - 1 = c2)
    || (r1 + 1 = r2 && c1 = c2)
  then true
  else false

let middle r1 c1 r2 c2 =
  if
    (r1 - 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 - 1 = c2)
    || (r1 + 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 + 1 = c2)
  then true
  else false

(* Takes in valid_list and checks if the user's input is a valid move.
   Valid moves are within the board's boundaries and are directly 1
   point up, down, left right, from the initial point *)
let valid_move int_list r c =
  let r1 = List.nth int_list 0 in
  let c1 = List.nth int_list 1 in
  let r2 = List.nth int_list 2 in
  let c2 = List.nth int_list 3 in
  (* Checks general bounds *)
  if in_bounds int_list r c = false then false (* Checks top left*)
  else if r1 = 0 && c1 == 0 then
    if top_left r2 c2 = false then false
    else true (* Checks bottom right *)
  else if r1 = r && c1 = c then
    if bottom_right r1 c1 r2 c2 = false then false else true
  else if r1 = r && c1 = 0 then
    if bottom_left r1 c1 r2 c2 = false then false else true
  else if r1 = 0 && c1 = c then
    if top_right r1 c1 r2 c2 = false then false else true
  else if r1 = 0 then if top_side c1 r2 c2 = false then false else true
  else if r1 = r then
    if bottom_side r1 c1 r2 c2 = false then false else true
  else if c1 = 0 then if left_side r1 r2 c2 = false then false else true
  else if c1 = c then
    if right_side r1 c1 r2 c2 = false then false else true
  else if middle r1 c1 r2 c2 = false then false
  else true

(* need to check if list is not length 4*)
let parse s = failwith "TODO"
