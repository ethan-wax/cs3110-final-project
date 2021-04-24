open Board

type t = string * string

let name player = fst player

let color player = snd player

let create_player name color = (name, color)
