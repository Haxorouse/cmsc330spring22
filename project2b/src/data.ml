open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
    | IntLeaf -> IntNode(x, IntLeaf, IntLeaf, IntLeaf)
    | IntNode (low, IntLeaf, _, _) -> 
      if x < low then IntNode (x, low, IntLeaf, IntLeaf, IntLeaf)
      else if x > low then IntNode (low, x, IntLeaf, IntLeaf, IntLeaf)
      else t
    | IntNode (low, high, fst, scd, thd) ->
      if x < low then int_insert x fst
      else if x > high then int_insert x thd
      else if x > low && x < high then int_insert x scd
      else t;;

let rec int_mem x t =
  match t with
    | IntLeaf -> false
    | IntNode (low, IntLeaf, _, _) -> if x = low then true else false
    | IntNode (low, high, fst, scd, thd) -> 
      if x = low || x = high then true
      else if x < low then int_mem x fst
      else if x > high then int_mem x thd
      else int_mem x scd;;

let rec int_size t =
   match t with
    | IntLeaf -> 0
    | IntNode (low, IntLeaf, _, _) -> 1
    | IntNode (low, high, fst, scd, thd) ->
      2 + int_size fst + int_size scd + int_size thd;;

let rec int_max t =
  match t with
    | IntLeaf -> raise (Invalid_argument("int_max"))
    | IntNode (low, IntLeaf, _, _) -> low
    | IntNode (low, high, _, _, IntLeaf) -> high
    | IntNode (low, high, _, _, thd) -> int_max thd;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = 
  failwith "unimplemented"

let rec map_contains k t = 
  failwith "unimplemented"

let rec map_get k t =
  failwith "unimplemented"

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = unit

let empty_table : lookup_table = ()

let push_scope (table : lookup_table) : lookup_table = 
  failwith "unimplemented"

let pop_scope (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let add_var name value (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let rec lookup name (table : lookup_table) =
  failwith "unimplemented"