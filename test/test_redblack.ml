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

let max_size = 1000

let random_tree () =
  let size = Random.int max_size in
  let values = List.init size (fun _ -> Random.int 1000) in
  make_rb cmp values

let flatten_tests = "Flatten" >::: [
  "Flatten: Random Tree" >:: (fun _ ->
    let tree = random_tree() in 
    let flattened = flatten tree in 
    let sorted = List.sort compare flattened in 
    assert_equal sorted flattened
    );

  "Flatten: Empty Tree" >:: (fun _ ->
    let tree = Leaf in
    let flattened = flatten tree in 
    assert_equal [] flattened
    );

  "Flatten: Single Node Tree" >:: (fun _ ->
    let tree = Node (Black, 10, Leaf, Leaf) in 
    let flattened = flatten tree in 
    assert_equal [10] flattened
    );

];;

let treemap_tests = "Treemap" >::: [
  "Treemap: Random Tree" >:: (fun _ ->
    let tree = random_tree () in 
    let squared = treemap (fun x -> x * x ) tree in 
    let sorted = List.sort compare squared in 
    assert_equal sorted squared
    );

  "Treemap: Empty Tree" >:: (fun _ ->
    let tree = Leaf in 
    let result = treemap (fun x -> x * x) tree in 
    assert_equal [] result
    );
  
    "Treemap: Single Node Tree" >:: (fun _ ->
      let tree = Node (Black, 10, Leaf, Leaf) in 
      let squared = treemap (fun x -> x * x) tree in 
      assert_equal [100] squared
      );
];;




print_string "Running tests";;
let _ = run_test_tt_main insert_tests;;
let _ = run_test_tt_main delete_tests;;
let _ = run_test_tt_main flatten_tests;;
let _ = run_test_tt_main treemap_tests;;