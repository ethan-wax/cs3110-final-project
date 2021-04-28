open Board

type t = {
  players : string * string;
  score : int * int;
  board : Board.t;
}

type move = string

let go brd plyr mv = failwith "TODO"
