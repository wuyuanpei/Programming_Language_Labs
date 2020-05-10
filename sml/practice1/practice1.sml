(* Richard Wu *)

exception NotYetImplemented

fun alternate(nums : int list) : int = 
	if nums = nil
	then 0
	else hd nums - alternate(tl nums)

fun min_max(nums : int list) : int*int = 
	if nums = nil
	then (0, 0)
	else
		let 
		val r = min_max(tl nums)
		val head = hd nums
		in
		if r = (0, 0)
		then (head, head)
		else if head < #1 r
		then (head, #2 r)
		else if head > #2 r
		then (#1 r, head)
		else r
		end
fun help_cumsum(nums : int list, sum : int ) : int list = 
	if null nums
	then []
	else
		let 
		val r = help_cumsum(tl nums, sum + hd nums)
		in
		(sum + hd nums)::r
		end

fun cumsum(nums : int list) : int list =
	help_cumsum(nums, 0)

fun greeting(name : string option) : string =
	if isSome name
	then "Hello there, "^valOf name^"!"
	else "Hello there, you!"

fun repeat(nums : int list, counts : int list) : int list = 
	if null counts
	then []
	else
	let
		val l = repeat(tl nums, tl counts)
		fun repeat_helper(t : int) : int list =
		if t = 0
		then l
		else 
		let 
		val l2 = repeat_helper(t - 1)
		in
		hd nums::l2
		end
	in
		repeat_helper(hd counts)
	end
		
fun addOpt(a : int option, b : int option) : int option = 
	raise NotYetImplemented
	
fun addAllOpt(opts : int option list) : int option = 
	raise NotYetImplemented

fun any(bools : bool list) : bool = 
	raise NotYetImplemented

fun all(bools : bool list) : bool = 
	raise NotYetImplemented
	
fun zip(a : int list, b : int list) : (int * int) list= 
	raise NotYetImplemented

fun zipRecycle(a : int list, b : int list) : (int * int) list= 
	raise NotYetImplemented

fun zipOpt(a_list : int list, b_list : int list) : (int*int) list option =
	raise NotYetImplemented

fun lookup(dictionary : (string*int) list, key : string) : int option = 
	raise NotYetImplemented

(* note: return (negs, nats) *)
fun splitup(ns : int list) : (int list * int list) = 
	raise NotYetImplemented

(* note: return (lower, upper) *)
fun splitAt(ns : int list, threshold : int) : (int list * int list) = 
	raise NotYetImplemented

fun isSorted(ns : int list) : bool =
	raise NotYetImplemented

fun isAnySorted(ns : int list) : bool =
	raise NotYetImplemented

fun sortedMerge(a_list : int list, b_list : int list) : int list =
	raise NotYetImplemented

fun qsort(ns : int list) : int list =
	raise NotYetImplemented

fun divide(ns : int list) : (int list * int list) =
	raise NotYetImplemented

fun not_so_quick_sort(ns : int list) : int list =
	raise NotYetImplemented

fun fullDivide(k : int, n : int) : int * int = 
	raise NotYetImplemented

fun factorize(n : int) : (int*int) list = 
	raise NotYetImplemented

fun multiply(factors : (int*int) list) : int = 
	raise NotYetImplemented

fun all_products(factors : (int*int) list) : int list =
	raise NotYetImplemented
