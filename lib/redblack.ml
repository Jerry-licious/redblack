type ordering =
  | LessThan
  | Equal
  | GreaterThan

type colour = Red | Black
type 'a rbtree = Node of colour * 'a * 'a rbtree * 'a rbtree  | Leaf

(* A function that compares two values. 
For the purposes of this program, it must be a linear order, that is:
1. a <= a for all (a: 'a).
2. If a <= b and b <= c, then a <= c.
3. If a <= b and b <= a, then a = b.
4. For any ((a, b): ('a * 'a)), either a <= b or b <= a. *)
type 'a comparator = 'a -> 'a -> ordering
(* search in RBT *)
let rec search (cmp: 'a comparator) (t: 'a rbtree) (value: 'a): ('a rbtree option) = match t with
| Leaf -> None
| Node (_, v, left, right) -> match (cmp value v) with
  | LessThan -> search cmp left value
  | GreaterThan -> search cmp right value
  | Equal -> Some t

(* Okasaki's balance operation that rotates the RBT so that the red node bubbles up *)
(* we handle all the cases where there is two successive reds, and put them next to each other *)
(* Refer to the diagram in https://www.cambridge.org/core/services/aop-cambridge-core/content/view/471C92AF3D431403FEE6C66FE070C492/S0956796814000227a.pdf/deletion-the-curse-of-the-red-black-tree.pdf for more details *)

let balance (c: colour) (value: 'a) (left: 'a rbtree) (right: 'a rbtree) = match (c,value,left,right) with
| (Black, z, Node (Red, y, Node (Red, x, a, b), c), d)
| (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d)
| (Black, x, a, Node (Red, z, Node (Red, y, b, c), d))
| (Black, x, a, Node (Red, y, b, Node (Red,z, c,d )))-> 
    let left_sub = Node (Black, x, a,b) in
    let right_sub = Node (Black, z, c, d) in
    Node (Red,y, left_sub, right_sub)
| _ -> Node (c, value, left, right)

let blacken (node: 'a rbtree) = match node with
| Node (Red, v, left, right) -> Node (Black, v, left, right)
| _ -> node
(* Okasaki's insert in RBT *)
let insert (cmp: 'a comparator) (t: 'a rbtree) (value: 'a) = 
  let rec helper (subtree: 'a rbtree) (sc: 'a rbtree -> 'a rbtree) = match subtree with
  | Leaf -> sc (Node (Red, value, Leaf, Leaf))
  | Node (colour, v, left, right) -> match (cmp value v) with
    | LessThan -> helper left (fun bleft -> sc (balance colour v bleft right))
    | GreaterThan -> helper right (fun bright -> sc (balance colour v left bright))
    | Equal -> sc subtree
in
blacken (helper t (fun x -> x))

let print_node (node : int rbtree) = 
  let rec helper (node: int rbtree) (level: int) (increment: int) = 
    match node with
    | Leaf -> ""
    | Node (c, v, left, right) -> let nodeString = (String.make level '-') ^ (string_of_int v) ^ (if c = Red then " R" else "") in
    let leftContent = if left != Leaf then "\n" ^ (helper left (level+increment) increment) else "" in
    let rightContent = if right != Leaf then "\n" ^ helper right (level+increment) increment else "" in
    String.concat "" [nodeString;leftContent;rightContent]
  in
  print_string (helper node 0 2)

let cmp a b = let cmp_result = Stdlib.compare a b in
match cmp_result with
| 0 -> Equal
| _ when cmp_result < 0 -> LessThan
| _ -> GreaterThan

(* validate validates if a given rbtree satisfies the invariants *)
exception BadRBLocalVariant;;
exception BadRBGlobalVariant;;
let validate (t: 'a rbtree) = 
  (* helper returns the black rank of the node, provided it is equal, otherwise it raises an exception *)
  let check_not_red (t: 'a rbtree) = match t with
  | Node (Red, _, _, _) -> raise BadRBLocalVariant
  | _ -> ()
in
  let rec helper (t: 'a rbtree) = match t with
  | Leaf -> 0
  | Node (c, _, l, r) -> 
    let left_rank = helper l in
    let right_rank = helper r in
    if c = Red then
      (
      check_not_red l;
      check_not_red r
      )
    else ();
    if left_rank = right_rank then left_rank+(if c = Black then 1 else 0) else (raise BadRBGlobalVariant)
    in
    helper t

let rec flatten (t: 'a rbtree) : 'a list =
  match t with
  | Leaf -> []
  | Node (_, v, l, r) -> 
    let left_list = flatten l in
    let right_list = flatten r in
    left_list @ [v] @ right_list

let treemap (f: 'a -> 'b)(t: 'a rbtree): 'b list = List.map f (flatten t)

let make_rb (cmp: 'a comparator) (values: 'a list) =
  let reducer t value = insert cmp t value in
  List.fold_left reducer Leaf values

type 'a rb = {
  insert: 'a -> unit;
  insert_list: 'a list -> unit;
  (*print: unit -> unit;*)
  remove: 'a -> bool;
  get_root: unit -> 'a rbtree;
  search: 'a -> 'a option;
  validate: unit -> int;
  lazy_count: unit -> int;
};;

let new_rb_with_root (cmp: 'a comparator) (tree: 'a rbtree) = 
  let t = ref tree in
  let count = ref 0 in
  
  let deleteTable = Hashtbl.create 1000 in
  let cleanup () = let deletedCount = Hashtbl.length deleteTable in if 2 * deletedCount > !count then
    (* time to cleanup. get all the values that are not in deleteTable and add them a brand new Red Black tree *)
    (
      let remaining = List.filter (fun x -> not (Hashtbl.mem deleteTable x)) (flatten (!t)) 
      in let rebuilt = List.fold_left (insert cmp) Leaf remaining 
      in (
        t := rebuilt;
        count := List.length remaining;
        Hashtbl.clear deleteTable
      )
    ) in

  let remove_lazy x = match Hashtbl.find_opt deleteTable x with 
  | Some _ -> false
  | None -> (Hashtbl.add deleteTable x x; cleanup (); true) in
  let search_with_lazy x : 'a option = match Hashtbl.find_opt deleteTable x with
  | Some _ -> None
  | None -> match (search cmp !t x) with
    | Some (Node (_, v, _, _)) -> Some v
    | _ -> None 
  in
  let insert_with_lazy x = match Hashtbl.find_opt deleteTable x with
  | Some _ -> (Hashtbl.remove deleteTable x); !t
  | None -> (
    let result = search_with_lazy x in
    if result = None then
      count := !count + 1;
      insert cmp !t x
    ) in
  let insert_rb x = (t:= insert_with_lazy x) in
  let insert_list_rb xs = List.fold_left (fun _ x -> insert_rb x) () xs in
  {
    insert = insert_rb;
    insert_list = insert_list_rb;
    (*print = (fun () -> print_node !t);*)
    remove = remove_lazy;
    get_root = (fun () -> !t);
    search = search_with_lazy;
    validate = (fun () -> validate !t);
    lazy_count = (fun () -> Hashtbl.length deleteTable)
  };;
let new_rb (cmp: 'a comparator) = new_rb_with_root cmp Leaf


(* Where the key and value are independent from each other. *)
type ('k, 'v) keyed_rb = {
  insert: ('k * 'v) -> unit;
  insert_list: ('k * 'v) list -> unit;
  (*print: unit -> unit;*)
  remove: 'k -> bool;
  (*
  The old implementation of the RB tree only allowed storing keys for simplicity. 
  While it could have been made to accomodate keys and values at the same time, 
  the complexity added to the structure is greater compared to wrapping it around
  with some functional techniques. 
  We can easily convert a comparison on keys into a (k, v) pair comparator through
  a higher order function defined below. 

  The difficulty lies in *generating* the desired key value pair when performing 
  a lookup. 
  Mainly, while lookups only care about the key, we still need a valid instance of
  ('k * 'v) to do the lookup. To circumvent this, internally we use an instance of
  tree with ('k * 'v option) values, so (k, None) is always a valie lookup. 

  While it may be better to modify the original type, I find this a cool way of
  doing a functional programming trick. 
  *)
  get_root: unit -> ('k * 'v option) rbtree;
  search: 'k -> 'v option;
  validate: unit -> int;
};;

(* Converts a comparator that only checks keys to a comparator that checks a key value pair. *)
let wrap_key_comparator (key_comp: 'k comparator): ('k * 'v) comparator =
  (fun (x: ('k * 'v)) (y: ('k * 'v)) -> key_comp (fst x) (fst y))

let wrap_kv(pair: ('k * 'v)): ('k * 'v option) =
  let (k, v) = pair in (k, Some(v))

let new_keyed_rb (comp: 'k comparator): ('k, 'v) keyed_rb = 
  let comp = wrap_key_comparator comp in
  let tree: ('k * 'v option) rb = (new_rb comp) in {
    insert=(fun (pair: ('k * 'v)) -> tree.insert (wrap_kv pair));
    insert_list=(fun (pairs: ('k * 'v) list) -> tree.insert_list (List.map wrap_kv pairs));
    (*print=(fun () -> tree.print());*)
    remove=(fun (k: 'k) -> tree.remove (k, None));
    get_root=(fun () -> tree.get_root());
    search=(fun (k: 'k) -> (match tree.search (k, None) with
      | None -> None
      | Some(_, v) -> v)
    );
    validate=(fun () -> tree.validate())
  }

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


let rec random_ints (n: int) = if n = 0 then [] else (Random.int 1073741823)::(random_ints (n-1))
let random_inserts (n: int) = 
  let t = new_rb cmp and data = random_ints n in
  List.iter (fun x -> (t.insert x; validate_tree t;)) data;;

let filteri (p: int -> 'a -> bool) (els: 'a list): 'a list = 
  let helper (acc,current_index) el = if p current_index el then (acc@[el],current_index+1) else (acc,current_index) in
  fst (List.fold_left helper ([],0) els)

let random_inserts_lookups (n: int) =
  let t = new_rb cmp and data = random_ints n in
  List.iteri (fun i x -> (
    t.insert x; 
    List.iteri (fun j x -> (
      let result = t.search x in
      if j <= i then (if Option.is_none result then raise Missing else ())
      else (
        let previous = filteri (fun (index: int) (_: int) -> index <= i) data
        in if List.mem x previous then ()
        else if Option.is_some result then raise Present else ()
      ))
    )) data;
  ) data;;
let random_deletes (n: int) = 
  let t = new_rb cmp and data = random_ints n in (
    t.insert_list data;
    List.iter (fun x -> (
      if Random.float 1.0 < 0.8 then (
        ignore (t.remove x); validate_tree t
      )
      else ()
    )) data
  );;

  
let random_deletes_lookups (n: int) =
  let t = new_rb cmp and data = random_ints n in (
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