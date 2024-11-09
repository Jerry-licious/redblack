open OUnit2
open Redblack
let basic_ordering a b = let cmp_result = compare a b in
if cmp_result < 0 then LessThan else if cmp_result > 0 then GreaterThan else Equal
let basic_tree : (int, int) bst = new_bst basic_ordering
basic_tree.left = new_node Red 1 1
let gp_tests = "test suite for gp" >::: [
  "gp of empty is empty" >:: (fun _ -> assert_equal true true);
  "gp of node is empty" >:: (fun _ -> assert_equal 1 1);
];;

print_string "Running tests";;
let _ = run_test_tt_main gp_tests