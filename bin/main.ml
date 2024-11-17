open Redblack
open Benchmark
let () = print_string "Executing bin/main.ml\n";;

let make_benchmarks (f: int -> unit) (name: string) = 
  let nums = [100;200;300;400;500;600;700;800;900;1000;2000;3000;4000;5000;10000;20000;30000;40000;50000] in
  let mapper num = (name ^ " " ^ string_of_int num, f, num) in
  List.map mapper nums


let insert_benchmarks = make_benchmarks random_inserts "Random Insert"
let delete_benchmarks = make_benchmarks random_deletes "Random Delete"
let benchmarks = insert_benchmarks @ delete_benchmarks
let _ = latencyN 1000L benchmarks