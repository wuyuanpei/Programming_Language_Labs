use "binary_tree.sml";

(* Richard Wu *)
exception NotYetImplemented

fun max_height(t : 'a BinaryTree.tree) : int = 
  	case t of
	  BinaryTree.LEAF => 0
	| BinaryTree.BRANCH (l,n,r) => 	let 
										val x = max_height(l)
										val y = max_height(r)
									in
										if x > y
										then x + 1
										else y + 1
									end


fun sum_int_tree(t : int BinaryTree.tree) : int =
  	BinaryTree.fold_rnl (fn (x,y)=>x+y) 0 t
