open Board

(** Quadrap*)
let valid_moves = ref [||]

let get_empty_branch r1 c1 r2 c2 board =
  let string_move =
    string_of_int r1 ^ " " ^ string_of_int c1 ^ " " ^ string_of_int r2
    ^ " " ^ string_of_int c2 ^ " "
  in
  match Command.parse string_move board with
  | Legal _ -> (
      let tuple_move = ((r1, c1), (r2, c2)) in
      match Board.branch_filled tuple_move board with
      | true -> ()
      | false ->
          valid_moves := Array.append !valid_moves [| string_move |])
  | Illegal -> ()

let get_empty_branches board =
  let dim = Board.dimensions board in
  for r1 = 0 to fst dim do
    for c1 = 0 to snd dim do
      for r2 = 0 to fst dim do
        for c2 = 0 to snd dim do
          get_empty_branch r1 c1 r2 c2 board
        done
      done
    done
  done

let easy board =
  get_empty_branches board;
  let rand_index = Random.int (Array.length !valid_moves - 1) in
  let ai_move = Array.get !valid_moves rand_index in
  valid_moves := [||];
  ai_move

let list_to_tuple lst =
  match lst with
  | [ x; y; z; d ] -> ((x, y), (z, d))
  | _ -> failwith "unimp"

let edge_moves = ref [||]

let medium board =
  get_empty_branches board;
  for h = 0 to Array.length !valid_moves - 1 do
    let move_point_lst =
      String.split_on_char ' ' (Array.get !valid_moves h)
    in
    let move_filtered_lst =
      List.filter (fun a -> a <> "") move_point_lst
    in
    let tuple_move = list_to_tuple move_filtered_lst in
    (* If the move is on the edge/corner then add it to edge array else
       go to next element in array *)
    if
      (* Checking left, right, top, bottom sides *)
      let r1 = int_of_string (fst (fst tuple_move)) in
      let c1 = int_of_string (snd (fst tuple_move)) in
      let r2 = int_of_string (fst (snd tuple_move)) in
      let c2 = int_of_string (snd (snd tuple_move)) in
      let board_r = fst (Board.dimensions board) in
      let board_c = fst (Board.dimensions board) in
      (c1 = 0 && c2 = 0)
      || (c1 = board_c && c2 = board_c)
      || (r1 = 0 && r2 = 0)
      || (r1 = board_r && r2 = board_r)
    then
      edge_moves :=
        Array.append !edge_moves [| Array.get !valid_moves h |]
    else ()
  done;
  (* If there are no edge moves available, choose a random available
     move from the board *)
  if Array.length !edge_moves = 0 then (
    let rand_index = Random.int (Array.length !valid_moves - 1) in
    let ai_move = Array.get !valid_moves rand_index in
    valid_moves := [||];
    ai_move
    (* If there are edge moves available, choose a random move from
       edge_moves *))
  else
    let rand_index = Random.int (Array.length !edge_moves - 1) in
    let ai_move = Array.get !edge_moves rand_index in
    edge_moves := [||];
    valid_moves := [||];
    ai_move

let hard board = failwith "unimplemented"
