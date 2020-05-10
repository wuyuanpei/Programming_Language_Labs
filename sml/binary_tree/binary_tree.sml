(* TODO: use CM.make to avoid having to go up and down a directory??? *)
use "../binary_tree/binary_tree.sig";

(* Richard Wu *)
(* Dennis Cosgrove *)
structure BinaryTree :> BINARY_TREE = struct
  exception NotYetImplemented

  datatype 'a tree = LEAF | BRANCH of 'a tree * 'a * 'a tree

  (*
   * depth-first, in-order traversal
   * https://en.wikipedia.org/wiki/Tree_traversal#In-order_(LNR)
   * https://en.wikipedia.org/wiki/Tree_traversal#In-order_(RNL)
   *)
  datatype traversal_order = LNR | RNL 

  fun insert (compare_function:(('a * 'a) -> order)) (t:'a tree) (item:'a) : 'a tree =
    	case t of
        LEAF => BRANCH (LEAF, item, LEAF)
        | BRANCH (l,n,r) => case compare_function(item, n) of
                                EQUAL => BRANCH (l,n,r) (* Undefined Operation: do nothing *)
                                | LESS => BRANCH (insert compare_function l item, n, r)
                                | GREATER => BRANCH (l, n, insert compare_function r item)

  fun delete_min (t:'a tree) =
      case t of
        BRANCH(LEAF, value, rt) => (value, rt)
        | BRANCH(lt, value, rt) => case delete_min lt of
                                        (v, r) => (v, BRANCH(r, value, rt))

  fun remove_helper (t:'a tree) =
      case t of
        BRANCH (l, _, LEAF) => l
        | BRANCH (l, _, r) => case delete_min r of
                                (value, rt) => BRANCH (l,value,rt)

  fun remove (equal_less_greater_function:'a -> order) (t:'a tree) : 'a tree =
    	case t of
        LEAF => LEAF
        | BRANCH (l,n,r) => case equal_less_greater_function(n) of
                            EQUAL => remove_helper t
                            | LESS => BRANCH ((remove equal_less_greater_function l), n, r)
                            | GREATER => BRANCH (l, n, (remove equal_less_greater_function r))


  fun find (equal_less_greater_function : 'a -> order) (t:'a tree) : 'a option = 
      case t of
    	  LEAF => NONE
        | BRANCH (l,n,r) => case equal_less_greater_function(n) of
                            EQUAL => SOME n
                            | LESS => find equal_less_greater_function l
                            | GREATER => find equal_less_greater_function r

  fun to_first_and_last(order:traversal_order, left:'a tree, right:'a tree) =
    	case order of
        LNR => (left, right)
        | RNL => (right, left)
    
  fun find_order_hof (order:traversal_order) (predicate:'a->bool) (t:'a tree) : 'a option =
      case t of
          LEAF => NONE
        | BRANCH (l,n,r) => (let val (first, second) = to_first_and_last(order,l,r) in
                              case (find_order_hof order predicate first, predicate n, find_order_hof order predicate second) of
                                    (SOME s,_,_) => SOME s
                                  | (_,true,_) => SOME n
                                  | (_,_,SOME s) => SOME s
                                  | _ => NONE
                                  end)


  (* value restriction *)
  (* val find_lnr = find_order_hof LNR *)
  (* val find_rnl = find_order_hof RNL *)

  (* necessary function wrapping *)
  fun find_lnr (predicate:'a->bool) (t:'a tree) : 'a option = 
    find_order_hof LNR predicate t
  fun find_rnl (predicate:'a->bool) (t:'a tree) : 'a option = 
    find_order_hof RNL predicate t

  fun fold_order_hof (order:traversal_order) (f : 'a * 'b -> 'b) (init : 'b) (t : 'a tree) : 'b = 
    	case order of
        LNR => (case t of
                  LEAF => init
                  | BRANCH (l,n,r) => fold_order_hof order f (f(n, fold_order_hof order f init l)) r)

      | RNL => (case t of
                  LEAF => init
                  | BRANCH (l,n,r) => fold_order_hof order f (f(n, fold_order_hof order f init r)) l)

  (* value restriction *)
  (* val fold_lnr = fold_order_hof LNR *)
  (* val fold_rnl = fold_order_hof RNL *)

  (* necessary function wrapping *)
  fun fold_lnr f init t = 
    fold_order_hof LNR f init t

  fun fold_rnl f init t = 
    fold_order_hof RNL f init t

  fun filter (predicate : 'a -> bool) (t : 'a tree) : 'a tree = 
    	raise NotYetImplemented

  fun filter_civil_war_style (predicate : 'a -> bool) (t : 'a tree) : 'a tree = 
    	case t of
        LEAF => LEAF
        | BRANCH (l,n,r) => if predicate n 
                            then BRANCH (filter_civil_war_style predicate l, n, filter_civil_war_style predicate r)
                            else LEAF


  fun map_to_tree_of_questionable_validity (f : 'a -> 'b) (t : 'a tree) : 'b tree = 
    	case t of
        LEAF => LEAF
        | BRANCH (l,n,r) => BRANCH(map_to_tree_of_questionable_validity f l,f n,map_to_tree_of_questionable_validity f r)

  fun map_to_tree_hof (order:traversal_order) (compare_function:(('b * 'b) -> order)) (f : 'a -> 'b) (t : 'a tree) : 'b tree =
    	raise NotYetImplemented

  (* value restriction *)
  (* val map_to_tree_lnr = map_to_tree_hof LNR *)
  (* val map_to_tree_rnl = map_to_tree_hof RNL *)

  (* necessary function wrapping *)
  fun map_to_tree_lnr compare_function f t = 
    map_to_tree_hof LNR compare_function f t

  fun map_to_tree_rnl compare_function f t = 
    map_to_tree_hof RNL compare_function f t

  fun map_to_list_hof (order:traversal_order) (f : 'a -> 'b) (t : 'a tree) : 'b list =
    	raise NotYetImplemented

  (* value restriction *)
  (* val map_to_list_lnr = map_to_list_hof LNR *)
  (* val map_to_list_rnl = map_to_list_hof RNL *)

  (* necessary function wrapping *)
  fun map_to_list_lnr f t = 
    map_to_list_hof LNR f t

  fun map_to_list_rnl f t = 
    map_to_list_hof RNL f t

  fun to_string value_to_string t =
    case t of 
      LEAF => "LEAF"
    | BRANCH(left, a, right) => "BRANCH(" ^ (to_string value_to_string left) ^ "," ^ value_to_string(a) ^ "," ^ (to_string value_to_string right) ^ ")"

end (* struct *) 
