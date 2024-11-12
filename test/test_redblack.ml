open OUnit2
open Redblack
let basic_ordering a b = let cmp_result = compare a b in
if cmp_result < 0 then LessThan else if cmp_result > 0 then GreaterThan else Equal

(* Counts the number of nodes in the tree, returns (non-marked nodes, total nodes) *)
let count_nodes (tree: 'a rb): (int * int) =
  let rec count_nodes_in (accumulator: int) (subtree: 'a rbtree): int =
    match subtree with
    | Leaf -> accumulator
    | Node(_, _, left, right) -> count_nodes_in (count_nodes_in (accumulator + 1) left) right
  in let total = count_nodes_in 0 (tree.get_root())
  in (total, total)

(* Calculates the height of the tree (depth of the deepest node). *)
let height (tree: 'a rb): int =
  let rec height_from (subtree: 'a rbtree): int =
    match subtree with
    | Leaf -> 0
    | Node(_, _, left, right) -> (max (height_from left) (height_from right)) + 1
  in height_from (tree.get_root())

let rec random_ints n = if n = 0 then [] else (Random.int 1000)::(random_ints (n-1))
let random_test () = 
  let t = new_rb cmp in
  t.insert_list (random_ints 1000);
  t.print ();
  let _ = t.validate () in ()

let insert_tests = "test suite for insert" >::: [
  "random insertion of elements retains RB structure" >:: (fun _ -> random_test ());
];;

print_string "Running tests";;
let _ = run_test_tt_main insert_tests