type ordering =
  | LessThan
  | Equal
  | GreaterThan;;

type colour = Red | Black | DoubleBlack;;
type 'a rbtree = Node of colour * 'a * 'a rbtree * 'a rbtree  | Leaf;;
type 'a comparator = 'a -> 'a -> ordering;;
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
  let rec helper (subtree: 'a rbtree) = match subtree with
  | Leaf -> Node (Red, value, Leaf, Leaf)
  | Node (colour, v, left, right) -> match (cmp value v) with
    | LessThan -> balance colour v (helper left) right
    | GreaterThan -> balance colour v left (helper right)
    | Equal -> subtree
in
blacken (helper t)

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
      check_not_red l;
      check_not_red r;
    if left_rank = right_rank then left_rank+(if c = Black then 1 else 0) else (raise BadRBGlobalVariant)
    in
    helper t


(* TODO: implement flatten *)
exception NotImplemented
let flatten (t: 'a rbtree) : 'a list = raise NotImplemented
let treemap (f: 'a -> 'b)(t: 'a rbtree): 'b list = raise NotImplemented

let make_rb (cmp: 'a comparator) (values: 'a list) =
  let reducer t value = insert cmp t value in
  List.fold_left reducer Leaf values

type 'a rb = {
  insert: 'a -> unit;
  search: 'a -> 'a option;
  remove: 'a -> bool;
  root: 'a rbtree;
};;
let new_rb (cmp: 'a comparator) = 

  let t = ref Leaf in
  let count = ref 0 in
  
  let deleteTable = Hashtbl.create 1000 in
  let cleanup () = if 2 * (Hashtbl.length deleteTable) > !count then
    (* time to cleanup. will use it after treemap and flatten is properly implemented *)
    () in

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
  {
    insert = (fun x -> (t := insert_with_lazy x));
    remove = remove_lazy;
    root = !t;
    search = search_with_lazy;
  }

(*
open String
(* Result of a comparison. *)
(* A function that compares two values. 
For the purposes of this program, it must be a linear order, that is:
1. a <= a for all (a: 'a).
2. If a <= b and b <= c, then a <= c.
3. If a <= b and b <= a, then a = b.
4. For any ((a, b): ('a * 'a)), either a <= b or b <= a. *)
type 'a linear_ord = 'a -> 'a -> ordering;;

(* Inverts a given linear order. *)
let invert (f: 'a linear_ord): 'a linear_ord = (fun x y -> f y x);;

type node_colour = 
  | Black
  | Red;;

(* Represents a node in a binary search tree. *)
type ('k, 'v) bst_node = {
  mutable key: 'k;
  mutable value: 'v;
  mutable colour: node_colour;
  parent: ('k, 'v) bst_node option ref;
  left: ('k, 'v) bst_node option ref;
  right: ('k, 'v) bst_node option ref;
};;

(* Represents a binary search tree with a given order. *)
type ('k, 'v) bst = {
  insert: ('k * 'v) -> unit;
  search: 'k -> 'v option;
  remove: 'k -> 'v option;
  root: ('k,'v) bst_node option ref;
  print: unit -> unit;
};;

exception DuplicateKey;;
(* A root leaf cannot be removed using methods that operate on nodes. *)
exception RemoveRootLeaf;;
exception NotFound;;

let is_some(o: 'a option): bool = match o with
| None -> false
| Some _ -> true

let is_none(o: 'a option): bool = match o with
| None -> true
| Some _ -> false

let unwrap (o: 'a option): 'a = match o with
| None -> raise NotFound
| Some(v) -> v

(* apply a list of functions to an option type  *)
let rec apply (value: 'a option) (functions: ('a -> 'a option) list) = match value with
| None -> None
| Some x -> (match functions with
  | [] -> Some x
  | f::fs -> apply (f x) fs)

let new_node (colour: node_colour) (entry: ('k * 'v)): ('k, 'v) bst_node = let (k, v) = entry in {
    key=k;
    value=v;
    colour=colour;
    parent=ref None;
    left=ref None;
    right=ref None;
  } ;;

let is_leaf (node: ('k, 'v) bst_node) = 
  !(node.left) = None && !(node.right) = None

let is_root (node: ('k, 'v) bst_node) =
  !(node.parent) = None

(* Grandparent of a node. *)
let gp(node: ('k, 'v) bst_node): ('k, 'v) bst_node option = 
  match !(node.parent) with
  | None -> None
  | Some(parent) -> !(parent.parent)

let sibling(node: ('k, 'v) bst_node): ('k, 'v) bst_node option =
  match !(node.parent) with
  | None -> None
  | Some(parent) -> if (!(parent.left) = Some(node)) then !(parent.right)
                    else !(parent.left)

(* Sibling of parent *)
let uncle(node: ('k, 'v) bst_node): ('k, 'v) bst_node option =
  match !(node.parent) with
  | None -> None
  | Some(parent) -> sibling parent
let l (x : ('k,'v) bst_node) = !(x.left)
let r (x : ('k,'v) bst_node) = !(x.right)
let p (x : ('k,'v) bst_node) = !(x.parent)
let u (x : ('k,'v) bst_node) = uncle x

let is_left (x: ('k, 'v) bst_node) = (apply (Some x) [p;l]) = (Some x)
let is_right (x: ('k, 'v) bst_node) = (apply (Some x) [p;r]) = (Some x)
let rec fix(node: ('k, 'v) bst_node) =
  if is_some (gp node) && is_some (uncle node) && is_some (p node) && (unwrap !(node.parent)).colour = Red && (unwrap (uncle node)).colour = Red then
    (let grandparent = unwrap (gp node) 
      in (
        grandparent.colour <- Red;
        (unwrap !(grandparent.left)).colour <- Black;
        (unwrap !(grandparent.right)).colour <- Black;
        fix(grandparent);
      )
    )
  else if is_root node then ()
  else let parent = unwrap !(node.parent) 
    in if is_root parent then (
      let sib = unwrap (sibling node)
      in if sib.colour = Red then
    (      
      node.colour <- Black;
      sib.colour <- Black
    )
      else ())
    
    else if parent.colour = Black then ()
    else (* parent.colour = Red and uncle.colour = Black *)
    if (is_right node) 
      then
      (
      (* we want to perform a rotation to the left *)
      let parent = unwrap !(node.parent) in
        parent.right := !(node.left);
        if !(node.left) != None then ((unwrap !(node.left)).parent := Some parent);
        node.parent := !(parent.parent);
        (* check if it's root and update it somehow *)
        if is_some !(node.parent) then
          if is_left node then (unwrap !(node.parent)).left := Some node else (unwrap !(node.parent)).right := Some node
        else ();
        node.left := Some parent;
        parent.parent := Some node;
      )
      else (
      let parent = unwrap !(node.parent) in
        parent.left := !(node.right);
        if !(node.right) != None then ((unwrap !(node.right)).parent := Some parent);
        node.parent := !(parent.parent);
        (* check if it's root and update it somehow *)
        if is_some !(node.parent) then
          if is_left node then (unwrap !(node.parent)).left := Some node else (unwrap !(node.parent)).right := Some node
        else ();
        node.right := Some parent;
        parent.parent := Some node;
      )

        



        

        

        
  


(* Based on the given ordering, insert an entry as a child of the current node. *)
let rec insert_at (cmp: 'k linear_ord) (node: ('k, 'v) bst_node) (entry: ('k * 'v)) =
  let (key, _) = entry in 
    match cmp key node.key with
    | LessThan -> (match !(node.left) with
      | Some(left_child) -> insert_at cmp left_child entry
      | None -> let child = new_node Red entry in 
        child.parent := Some(node); 
        node.left := Some(child))
    | GreaterThan -> (match !(node.right) with
      | Some(right_child) -> insert_at cmp right_child entry
      | None -> let child = new_node Red entry in 
        child.parent := Some(node); 
        node.right := Some(child))
    | Equal -> raise DuplicateKey

(* Based on the given ordering, find a node with the given key. *)
let rec search_at (cmp: 'k linear_ord) (key: 'k) (node: ('k, 'v) bst_node): ('k, 'v) bst_node option =
  match cmp key node.key with
  | LessThan -> (match !(node.left) with
    | Some(left_child) -> search_at cmp key left_child
    | None -> None)
  | GreaterThan -> (match !(node.right) with
    | Some(left_child) -> search_at cmp key left_child
    | None -> None)
  | Equal -> Some(node)

(* Removes a leaf node from the tree. Only works if the node is a leaf. *)
let remove_leaf (node: ('k, 'v) bst_node): ('k * 'v) =
  match !(node.parent) with
    | None -> raise RemoveRootLeaf
    | Some(parent) -> (
      if !(parent.left) = Some(node) then parent.left := None
      else parent.right := None); (node.key, node.value)

(* Removes the minimum element from the current node while maintaining the tree structure. *)
let rec pop_minimum (cmp: 'k linear_ord) (node: ('k, 'v) bst_node): ('k * 'v) =
  if is_leaf node then remove_leaf node
  else (
    match !(node.left) with
    | Some(left) -> pop_minimum cmp left
    | None -> (
      (* Safe unwrap because we know this node isn't a leaf. *)
      let right = unwrap !(node.right) 
      and (current_key, current_value) = (node.key, node.value)
      in let (successor_key, successor_value) = pop_minimum cmp right 
        in node.key <- successor_key; node.value <- successor_value; (current_key, current_value)
    )
  )

let pop_maximum (cmp: 'k linear_ord) (node: ('k, 'v) bst_node): ('k * 'v) =
  pop_minimum (invert cmp) node

let remove_at (cmp: 'k linear_ord) (key: 'k) (node: ('k, 'v) bst_node): 'v option =
  match search_at cmp key node with
  | None -> None
  | Some(to_remove) -> (
    if is_leaf to_remove then let (_, v) = remove_leaf to_remove in Some(v)
    else let (_, removed_value) = (to_remove.key, to_remove.value) 
      and (successor_key, successor_value) = (
        match !(node.left) with
        | Some(left) -> pop_maximum cmp left
        | None -> pop_maximum cmp (unwrap !(node.right))
      ) in (to_remove.key <- successor_key; to_remove.value <- successor_value; Some(removed_value))
  )

(* returns a stringifed version of the node with only its value *)
let rec print_node (node : (int, int) bst_node option) = 
  let rec helper (node: (int, int) bst_node option) (level: int) (increment: int) = 
    match node with
    | None -> ""
    | Some x -> let nodeString = (String.make level '-') ^ (string_of_int x.key) ^ (if x.colour = Red then " R" else "") in
    let leftContent = if !(x.left) != None then "\n" ^ (helper !(x.left) (level+increment) increment) else "" in
    let rightContent = if !(x.right) != None then "\n" ^ helper !(x.right) (level+increment) increment else "" in
    concat "" [nodeString;leftContent;rightContent]
  in
  helper node 0 2


let new_bst (cmp: 'k linear_ord) = 
  let rootRef = ref None in
  let update_root () = 
    if !rootRef != None then
      let root = unwrap !rootRef in
      if !(root.parent) != None then
        rootRef := !(root.parent)
  in {
    insert=(fun (entry: ('k * 'v)) -> 
      match !rootRef with
      | None -> rootRef := Some(new_node Black entry)
      | Some(node) -> (
        insert_at cmp node entry;
        fix node;
        update_root ();
      )
    );
    search=( fun (key: 'k) -> 
      match !rootRef with
      | None -> None
      | Some(node) -> (
        match search_at cmp key node with
        | None -> None
        | Some(target_node) -> Some(target_node.value)
      )
    );
    remove=( fun (key: 'k) -> 
      match !rootRef with
      | None -> raise NotFound
      | Some(node) -> try (
        let ret = remove_at cmp key node in
        fix node;
        update_root ();
        ret
      )
        with RemoveRootLeaf -> (rootRef := None; Some(node.value))
    );
    print=(fun () -> print_string (print_node !rootRef); ());
    root=rootRef;
  } ;;


let set_parent (child: ('k, 'v ) bst_node option) (parent: ('k, 'v) bst_node) = try (unwrap child).parent := Some parent with NotFound -> ()
(* make node makes a node with its left and right children, making sure the parent ref is properly updated *)
let make_node (key: 'k) (value : 'v) (colour: node_colour) (left: ('k,'v) bst_node option) (right: ('k,'v) bst_node option): ('k,'v) bst_node = 
  let current_node = {key = key; value = value; colour = colour; parent = ref None; left = ref left; right = ref right } in
  set_parent left current_node;
  set_parent right current_node;
  current_node

let mnf (v : int) (left: ('k,'v) bst_node) (right: ('k,'v) bst_node) = make_node v v Red (Some left) (Some right)
let mnl (v: int) (left: ('k, 'v) bst_node) = make_node v v Red (Some left) None
let mnr (v: int) (right: ('k, 'v) bst_node) = make_node v v Red None (Some right)
let ml (v: int) = make_node v v Black None None


let sample_node = mnf 2 (mnl 1 (ml 0)) (mnr 3 (ml 4));;
(* print_string (print_node (Some sample_node));; *)

let simple_order (a : int) (b: int) = let cmp_result = Stdlib.compare a b in
if cmp_result < 0 then LessThan else if cmp_result > 0 then GreaterThan else Equal
let sample_bst = new_bst simple_order;;
sample_bst.insert (1,1);;
sample_bst.insert (2,2);;
sample_bst.insert (3,3);;
sample_bst.insert (4,4);;
sample_bst.print ();;
*)