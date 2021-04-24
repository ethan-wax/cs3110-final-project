open Board
open Command
open Player

type t = int

type state_result =
  | Valid of Board.t
  | Invalid

type move = int list

let go board player move = Valid board
