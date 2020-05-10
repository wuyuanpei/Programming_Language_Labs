(* STUDENT_NAME *)

(* credit to Charilaos Skiadas http://vault.hanover.edu/~skiadas/ for these practice problems *)

exception NotYetImplemented

fun compose_opt (f:'b->'c option) (g:'a->'b option) (x:'a) : 'c option = 
	raise NotYetImplemented

fun do_until (f:'a->'a) (predicate:'a->bool) (x:'a) : 'a =
	raise NotYetImplemented

fun factorial (n : int) : int =
	raise NotYetImplemented

fun fixed_point (f:''a->''a) (x:''a) : ''a =
	raise NotYetImplemented

fun map2 (f:'a->'b) (x1:'a,x2:'a) : ('b*'b) = 
	raise NotYetImplemented

fun app_all (f:'b->'c list) (g:'a->'b list) (x:'a) : 'c list =
	raise NotYetImplemented

fun foldr (f:'a*'b->'b) (acc:'b) (xs:'a list) : 'b =
	raise NotYetImplemented

fun partition (predicate:'a->bool) (xs:'a list) : ('a list*'a list) =
	raise NotYetImplemented

fun unfold (f:'a->('b*'a) option) (seed:'a) : 'b list =
	raise NotYetImplemented

fun map (f:'a->'b) (xs:'a list) : 'b list = 
	raise NotYetImplemented

fun filter (predicate:'a->bool) (xs:'a list) : 'a list = 
	raise NotYetImplemented


fun foldl (f:'a*'b->'b) (acc:'b) (xs:'a list) : 'b =
	raise NotYetImplemented

datatype 'a tree = LEAF | NODE of 'a * 'a tree * 'a tree

fun mapTree (f:'a->'b) (t:'a tree) : 'b tree =
	raise NotYetImplemented

fun foldTree (f:'a*'b->'b) (acc:'b) (t:'a tree) : 'b =
	raise NotYetImplemented

fun filterTree (predicate:'a->bool) (t:'a tree) : 'a tree =
	raise NotYetImplemented
