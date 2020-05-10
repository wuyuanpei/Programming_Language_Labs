(* STUDENT_NAME *)

(* credit to Charilaos Skiadas http://vault.hanover.edu/~skiadas/ for these practice problems *)

exception NotYetImplemented

(*
 * these types are used for pass_or_fail, has_passed, number_passed, and number_misgraded 
 *)
type student_id = int;
type grade = int (* must be in 0 to 100 range *);
type final_grade = { id : student_id, grade : grade option };

datatype pass_fail = pass | fail

fun pass_or_fail(fg : final_grade) : pass_fail =
	raise NotYetImplemented

fun has_passed(fg : final_grade) : bool = 
	raise NotYetImplemented

fun number_passed(fgs : final_grade list) : int =
	raise NotYetImplemented

fun number_misgraded(xs : (pass_fail * final_grade) list) : int = 
	raise NotYetImplemented

(*
 * these types are used for tree_height, sum_tree, and gardener 
 *)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height(t : 'a tree) : int =
	raise NotYetImplemented

fun sum_tree(t : int tree) : int =
	raise NotYetImplemented

fun gardener(t : flag tree) : flag tree = 
	raise NotYetImplemented

(*
 * http://sml-family.org/Basis/list.html
 *)

(* 
 * returns the last element of vs. 
 * It raises Empty if vs is nil. 
 *)
fun my_last(vs : 'a list) : 'a =
	raise NotYetImplemented

(* 
 * returns the first i elements of the list vs. 
 * It raises Subscript if i < 0 or i > length vs. 
 * We have take(vs, length vs) = l. 
 *)
fun my_take(vs : 'a list, i : int) : 'a list =
	raise NotYetImplemented

(* 
 * returns what is left after dropping the first i elements of the list vs. 
 * It raises Subscript if i < 0 or i > length vs. 
 * It holds that take(vs, i) @ drop(vs, i) = l when 0 <= i <= length l. 
 * We also have drop(vs, length vs) = []. 
 *)
fun my_drop(vs : 'a list, i : int) : 'a list =
	raise NotYetImplemented

(* 
 * returns the list that is the concatenation of all the lists in vss in order.
 * concat[vs1,vs2,...vsn] = vs1 @ vs2 @ ... @ vsn
 *)
fun my_concat(vss : 'a list list ) : 'a list =
	raise NotYetImplemented

(*
 * http://sml-family.org/Basis/option.html
 *)

(* 
 * returns v if opt is SOME(v); otherwise it returns default_value. 
 *)
fun my_getOpt(opt : 'a option, default_value : 'a) =
	raise NotYetImplemented

(*
 * The my_join function maps NONE to NONE and SOME(v) to v. 
 *)
fun my_join(opt_opt : 'a option option) : 'a option =
	raise NotYetImplemented

(*
 * this type and exception are used for is_positive, pred, nat_to_int, int_to_nat, add, sub, mult, and less_than
 *)
datatype nat = ZERO | SUCC of nat
exception Negative

fun is_positive(n : nat) : bool = 
	raise NotYetImplemented

fun pred(n : nat) : nat =
	raise NotYetImplemented

fun nat_to_int(n : nat) : int =
	raise NotYetImplemented

fun int_to_nat(i : int) : nat =
	raise NotYetImplemented

fun add(a:nat, b:nat) : nat =
	raise NotYetImplemented

fun sub(a:nat, b:nat) : nat =
	raise NotYetImplemented

fun mult(a:nat, b:nat) : nat =
	raise NotYetImplemented

fun less_than(a:nat, b:nat) : bool =
	raise NotYetImplemented

(*
 * this type is used for isEmpty, contains, toList
 *)
datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

fun isEmpty(s : intSet) : bool =
	raise NotYetImplemented

fun contains(s : intSet, v : int) : bool =
	raise NotYetImplemented

fun toList(s : intSet) : int list =
	raise NotYetImplemented
