(* Result of a comparison. *)
type ordering =
  | LessThan
  | Equal
  | GreaterThan;;

(* A function that compares two values. 
For the purposes of this program, it must be a linear order, that is:
1. a <= a for all (a: 'a).
2. If a <= b and b <= c, then a <= c.
3. If a <= b and b <= a, then a = b.
4. For any ((a, b): ('a * 'a)), either a <= b or b <= a. *)
type 'a linear_ord = 'a -> 'a -> ordering;;

(* Inverts a given linear order. *)
let invert (f: 'a linear_ord): 'a linear_ord = (fun x y -> f y x)


(* Represents a node in a binary search tree. *)
type ('k, 'v) bst_node = {
  mutable key: 'k;
  mutable value: 'v;
  parent: ('k, 'v) bst_node option ref;
  left: ('k, 'v) bst_node option ref;
  right: ('k, 'v) bst_node option ref;
};;

(* Represents a binary search tree with a given order. *)
type ('k, 'v) bst = {
  insert: ('k * 'v) -> unit;
  search: 'k -> 'v option;
  remove: 'k -> 'v option;
};;

exception DuplicateKey;;
(* A root leaf cannot be removed using methods that operate on nodes. *)
exception RemoveRootLeaf;;
exception NotFound;;

let new_node (entry: ('k * 'v)): ('k, 'v) bst_node = 
  let (k, v) = entry in {
    key=k;
    value=v;
    parent=ref None;
    left=ref None;
    right=ref None;
  };;

let is_leaf (node: ('k, 'v) bst_node) = 
  !(node.left) = None && !(node.right) = None

let is_root (node: ('k, 'v) bst_node) =
  !(node.parent) = None

(* Based on the given ordering, insert an entry as a child of the current node. *)
let rec insert_at (cmp: 'k linear_ord) (node: ('k, 'v) bst_node) (entry: ('k * 'v)) =
  let (key, value) = entry in 
    match cmp key node.key with
    | LessThan -> (match !(node.left) with
      | Some(left_child) -> insert_at cmp left_child entry
      | None -> let child = new_node entry in 
        child.parent := Some(node); 
        node.left := Some(child))
    | GreaterThan -> (match !(node.right) with
      | Some(right_child) -> insert_at cmp right_child entry
      | None -> let child = new_node entry in 
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
      let right = Option.get !(node.right) 
      and (current_key, current_value) = (node.key, node.value)
      in let (successor_key, successor_value) = pop_minimum cmp right 
        in node.key <- successor_key; node.value <- successor_value; (current_key, current_value)
    )
  )

let rec pop_maximum (cmp: 'k linear_ord) (node: ('k, 'v) bst_node): ('k * 'v) =
  pop_minimum (invert cmp) node

let remove_at (cmp: 'k linear_ord) (key: 'k) (node: ('k, 'v) bst_node): 'v option =
  match search_at cmp key node with
  | None -> None
  | Some(to_remove) -> (
    if is_leaf to_remove then let (k, v) = remove_leaf to_remove in Some(v)
    else let (removed_key, removed_value) = (to_remove.key, to_remove.value) 
      and (successor_key, successor_value) = (
        match !(node.left) with
        | Some(left) -> pop_maximum cmp left
        | None -> pop_maximum cmp (Option.get !(node.right))
      ) in (to_remove.key <- successor_key; to_remove.value <- successor_value; Some(removed_value))
  )

let new_bst (cmp: 'k linear_ord) = 
  let root = ref None in {
    insert=(fun (entry: ('k * 'v)) -> 
      match !root with
      | None -> root := Some(new_node entry)
      | Some(node) -> insert_at cmp node entry
    );
    search=( fun (key: 'k) -> 
      match !root with
      | None -> None
      | Some(node) -> (
        match search_at cmp key node with
        | None -> None
        | Some(target_node) -> Some(target_node.value)
      )
    );
    remove=( fun (key: 'k) -> 
      match !root with
      | None -> raise NotFound
      | Some(node) -> try remove_at cmp key node 
        with RemoveRootLeaf -> (root := None; Some(node.value))
    );
  };;
