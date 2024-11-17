open Redblack
open Benchmark
let () = print_string "Executing bin/main.ml\n";;

let rec range start stop inc = if start > stop then [] else start::(range (start+inc) stop inc)

let make_insert_benchmark n = 
  let t = new_rb cmp and data = random_ints n in
  let new_data = random_ints 1000 in
  t.insert_list data;
  let saved_root = t.get_root () in
  fun () -> (
    let current_t = new_rb_with_root cmp saved_root in
    current_t.insert_list new_data
  )
let make_delete_benchmark n = 
  let t = new_rb cmp and data = random_ints n in
  t.insert_list data;
  let saved_root = t.get_root () in
  fun () -> (
    let current_t = new_rb_with_root cmp saved_root in
    List.iter (fun x -> (
      if Random.float 1.0 < 0.8 then (
        ignore (current_t.remove x);
      )
      else ()
    )) data
  )

let make_benchmarks nums (make_f: int -> unit -> unit) (name: string) = 
  let mapper num = (name ^ " " ^ string_of_int num, make_f num, ()) in
  List.map mapper nums

let num_range = range 10000 200000 5000
let insert_benchmarks = make_benchmarks num_range make_insert_benchmark "Random Insert"
let _ = latencyN 10000L insert_benchmarks;;
let delete_benchmarks = make_benchmarks num_range make_delete_benchmark "Random Delete"
let _ = latencyN 100L delete_benchmarks;;