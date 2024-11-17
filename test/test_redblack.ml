open OUnit2
open Redblack



let test_size = 1000
let insert_tests = "Random Insertions" >::: [
  "Insertion: Structure" >:: (fun _ -> random_inserts test_size);
  "Insertion: Lookups" >:: (fun _ -> random_inserts_lookups test_size);
];;


let delete_tests = "Random Deletions" >::: [
  "Deletion: Structure" >:: (fun _ -> random_deletes test_size);
  "Deletion: Search" >:: (fun _ -> random_deletes_lookups test_size);
];;

print_string "Running tests";;
let _ = run_test_tt_main insert_tests;;
let _ = run_test_tt_main delete_tests;;