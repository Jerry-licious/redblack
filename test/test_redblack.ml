open OUnit2
open Redblack
let basic_ordering a b = let cmp_result = compare a b in
if cmp_result < 0 then LessThan else if cmp_result > 0 then GreaterThan else Equal

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