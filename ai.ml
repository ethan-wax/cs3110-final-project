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

let medium board =
  get_empty_branches board;
  let rand_index = Random.int (Array.length !valid_moves - 1) in
  let ai_move = Array.get !valid_moves rand_index in
  valid_moves := [||];
  ai_move

let hard board = failwith "unimplemented"
