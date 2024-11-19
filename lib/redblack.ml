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

(* blacken accepts a node and outputs a copy of the node, but red *)
let blacken (node: 'a rbtree) = match node with
| Node (Red, v, left, right) -> Node (Black, v, left, right)
| _ -> node
(* insert implements Okasaki's insertion algorithm. It uses a success continuation to bubble up the red node to the root, and then blacken it *)
let insert (cmp: 'a comparator) (t: 'a rbtree) (value: 'a) = 
  let rec helper (subtree: 'a rbtree) (sc: 'a rbtree -> 'a rbtree) = match subtree with
  | Leaf -> sc (Node (Red, value, Leaf, Leaf))
  | Node (colour, v, left, right) -> match (cmp value v) with
    | LessThan -> helper left (fun bleft -> sc (balance colour v bleft right))
    | GreaterThan -> helper right (fun bright -> sc (balance colour v left bright))
    | Equal -> sc subtree
in
blacken (helper t (fun x -> x))

(* print_node is a helper function that prints an integer red black tree *)
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

(* cmp is a standard comparator for int rbtrees  *)
let cmp a b = let cmp_result = Stdlib.compare a b in
match cmp_result with
| 0 -> Equal
| _ when cmp_result < 0 -> LessThan
| _ -> GreaterThan

exception BadRBGlobalVariant;;
(* validate validates if a given rbtree satisfies the local (no successive red nodes) and global invariants (black height is the same across all paths) *)
exception BadRBLocalVariant;;
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

(* flatten takes an rbtree and outputs an inorder traversal of the rbtree  *)
let rec flatten (t: 'a rbtree) : 'a list =
  match t with
  | Leaf -> []
  | Node (_, v, l, r) -> 
    let left_list = flatten l in
    let right_list = flatten r in
    left_list @ [v] @ right_list

(* treemap maps a function f to an rbtree and returns it in an inorder list *)
let treemap (f: 'a -> 'b)(t: 'a rbtree): 'b list = List.map f (flatten t)

(* make_rb is a helper method to make an rbtree based on a list of values *)
let make_rb (cmp: 'a comparator) (values: 'a list) =
  let reducer t value = insert cmp t value in
  List.fold_left reducer Leaf values

(* the rb record supports insertion, multiple insertion, removal, and some auxiliary methods such as getting the root, validating the tree, and getting the lazy count *)
(* our complete implementation of the rb tree uses lazy-delete, which has an amortized performance of O(logn) *)
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

(* new_rb_with_root is a function that takes a comparator and a rbtree node and produces a rb record for mutable insertion and deletion *)
let new_rb_with_root (cmp: 'a comparator) (tree: 'a rbtree) = 
  let t = ref tree in
  let count = ref 0 in
  
  let deleteTable = Hashtbl.create 200000 in
  (* cleanup is invoked when the number of deleted elements is greater than the number of existing elements. the rb tree is created anew with only the non-deleted elements *)
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

  (* remove_lazy lazily removes an element by putting it inside the hashtable *)
  let remove_lazy x = match Hashtbl.find_opt deleteTable x with 
  | Some _ -> false
  | None -> (Hashtbl.add deleteTable x x; cleanup (); true) in
  (* search_with_lazy searches an element, making sure that it is not in the delete hashtable *)
  let search_with_lazy x : 'a option = match Hashtbl.find_opt deleteTable x with
  | Some _ -> None
  | None -> match (search cmp !t x) with
    | Some (Node (_, v, _, _)) -> Some v
    | _ -> None 
  in
  (* insert_with_lazy inserts an element in the rb tree while keeping into account the delete hashtable *)
  let insert_with_lazy x = match Hashtbl.find_opt deleteTable x with
  | Some _ -> (Hashtbl.remove deleteTable x); !t
  | None -> (
    let result = search_with_lazy x in
    if result = None then
      count := !count + 1;
      insert cmp !t x
    ) in
    (* insert_rb inserts a value in place *)
  let insert_rb x = (t:= insert_with_lazy x) in
  (* insert_list_rb inserts a list of values in place *)
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
(* new_rb is a shorthand for creating an empty rb tree *)
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

(* wrap_kv wraps a tuple into a tuple to be used by the keyed_rb *)
let wrap_kv(pair: ('k * 'v)): ('k * 'v option) =
  let (k, v) = pair in (k, Some(v))

(* keyed_rb creates a keyable red black tree that performs comparison based on keys and allows storing values *)
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
