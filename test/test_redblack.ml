open OUnit2
open Redblack


let basic_ordering a b = let cmp_result = compare a b in
if cmp_result < 0 then LessThan else if cmp_result > 0 then GreaterThan else Equal

exception TooShort;;
exception TooTall;;
exception TooLazy;; (* Too many marked nodes. *)


exception Missing;; (* Search does not return an element when it should be there. *)
exception Present;; (* Search returns something when it shouldn't be there. *)


(* Counts the number of nodes in the tree. *)
let count_nodes (tree: 'a rb): int =
  let rec count_nodes_in (accumulator: int) (subtree: 'a rbtree): int =
    match subtree with
    | Leaf -> accumulator
    | Node(_, _, left, right) -> count_nodes_in (count_nodes_in (accumulator + 1) left) right
  in count_nodes_in 0 (tree.get_root())

(* Calculates the height of the tree (depth of the deepest node). *)
let height (tree: 'a rb): int =
  let rec height_from (subtree: 'a rbtree): int =
    match subtree with
    | Leaf -> 0
    | Node(_, _, left, right) -> (max (height_from left) (height_from right)) + 1
  in height_from (tree.get_root())


let validate_height (tree: 'a rb) =
  let total = count_nodes tree
  and height = height tree
  in let log2_total = (int_of_float (ceil (log (float_of_int (total + 1)))) + 1)
  in if height > log2_total * 2 then (print_int height; print_endline ""; print_int log2_total; raise TooTall)
     else if height < log2_total - 1 then raise TooShort
     else ()

let validate_occupancy (tree: 'a rb) =
  let total = count_nodes tree in
  if (tree.lazy_count()) * 2 > total then raise TooLazy else ()

let validate_tree (tree: 'a rb): unit = 
  (validate_height tree; validate_occupancy tree; ignore (tree.validate()))


let rec random_ints n = if n = 0 then [] else (Random.int 1000)::(random_ints (n-1))
let random_inserts () = 
  let t = new_rb cmp and data = random_ints 1000 in
  List.iter (fun x -> (t.insert x; validate_tree t;)) data;;

let random_inserts_lookups () =
  let t = new_rb cmp and data = random_ints 1000 in
  List.iteri (fun i x -> (
    t.insert x; 
    List.iteri (fun j x -> (
      let result = t.search x in
      if j <= i then (if Option.is_none result then raise Missing else ())
      else (
        let previous = List.filteri (fun (index: int) (_: int) -> index <= i) data
        in if List.mem x previous then ()
        else if Option.is_some result then raise Present else ()
      ))
    )) data;
  ) data;;


let insert_tests = "Random Insertions" >::: [
  "Insertion: Structure" >:: (fun _ -> random_inserts ());
  "Insertion: Lookups" >:: (fun _ -> random_inserts_lookups ());
];;

let random_deletes () = 
  let t = new_rb cmp and data = random_ints 1000 in (
    t.insert_list data;
    List.iter (fun x -> (
      if Random.float 1.0 < 0.8 then (
        ignore (t.remove x); validate_tree t
      )
      else ()
    )) data
  );;

  
let random_deletes_lookups () =
  let t = new_rb cmp and data = random_ints 1000 in (
    t.insert_list data;
    List.iter (fun x -> (
      if Option.is_some (t.search x) then (
        if Random.bool() then (
          ignore (t.remove x); 
          if Option.is_some (t.search x) 
            then raise Present else ()
        )
        else ()
      ) else ()
    )) data
  );;

let delete_tests = "Random Deletions" >::: [
  "Deletion: Structure" >:: (fun _ -> random_deletes ());
  "Deletion: Search" >:: (fun _ -> random_deletes_lookups ());
];;

print_string "Running tests";;
let _ = run_test_tt_main insert_tests;;
let _ = run_test_tt_main delete_tests;;