open OUnit2
open Board
open Command

let extract_t (st : result) =
  match st with Legal t -> t | Illegal -> raise (failwith "no info")

let valid_parse_test
    (name : string)
    (move : string)
    (board : Board.t)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output (extract_t (parse move board))

let fail_parse_test
    (name : string)
    (move : string)
    (board : Board.t)
    (expected_output : Command.result) : test =
  name >:: fun _ -> assert_equal expected_output (parse move board)

let default_board = make_board (4, 5)

let square_board = make_board (6, 6)

let one_by_one = make_board (1, 1)

let dimension_test (name : string) (board : Board.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (dimensions board)

let get_branch_test
    (name : string)
    (points : (int * int) * (int * int))
    (board : Board.t)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (get_branch points board)

let board_tests =
  [
    dimension_test "default board" default_board (4, 5);
    dimension_test "square" square_board (6, 6);
    dimension_test "one-by-one" one_by_one (1, 1);
  ]

let command_tests =
  [
    (* Four corners then going either up or down *)
    valid_parse_test "top left down" "0 0 1 0" default_board
      [ 0; 0; 1; 0 ];
    valid_parse_test "bottom right up" "4 5 3 5" default_board
      [ 4; 5; 3; 5 ];
    valid_parse_test "bottom left up" "4 0 3 0" default_board
      [ 4; 0; 3; 0 ];
    valid_parse_test "top right down" "0 5 1 5" default_board
      [ 0; 5; 1; 5 ];
    (* Four sides then going either right or left *)
    valid_parse_test "top side right" "0 1 0 2" default_board
      [ 0; 1; 0; 2 ];
    valid_parse_test "bottom side left" "4 4 4 3" default_board
      [ 4; 4; 4; 3 ];
    valid_parse_test "left side right" "1 0 1 1" default_board
      [ 1; 0; 1; 1 ];
    valid_parse_test "right side left" "1 5 1 4" default_board
      [ 1; 5; 1; 4 ];
    (* in the middle*)
    valid_parse_test "middle down" "2 3 3 3" default_board
      [ 2; 3; 3; 3 ];
    (* Failing tests *)
    fail_parse_test "negative bounds" "-1 0 1 0" default_board Illegal;
    fail_parse_test "non numbers in input" "words" default_board Illegal;
    fail_parse_test "too far away" "0 0 2 0" default_board Illegal;
    fail_parse_test "diagonal move " "0 0 1 1" default_board Illegal;
  ]

let suite =
  "test suite for final project" >::: List.flatten [ command_tests ]

let _ = run_test_tt_main suite
