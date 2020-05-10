use "../unit_test/unit_test.sml";
use "practice3.sml";

val _ = let
	fun tree_to_string value_to_string t =
		case t of
		  LEAF => "LEAF"
		| NODE(v,left,right) => "NODE (" ^ value_to_string(v) ^ "," ^ (tree_to_string value_to_string left)  ^ "," ^ (tree_to_string value_to_string right) ^ ")"

	val int_tree_to_string = tree_to_string Int.toString
	val assertEquals_IntTree = UnitTest.assertEquals int_tree_to_string

	val _ = print "\n\n\n>>> begin test error output\n"

	val _ = UnitTest.enter("compose_opt")
	val _ = UnitTest.assertEquals_IntOption(NONE, compose_opt (fn x => NONE) (fn y => NONE) 425)
	val _ = UnitTest.assertEquals_IntOption(NONE, compose_opt (fn x => NONE) (fn y => SOME y) 425)
	val _ = UnitTest.assertEquals_IntOption(NONE, compose_opt (fn x => SOME x) (fn y => NONE) 425)
	val _ = UnitTest.assertEquals_IntOption(SOME 425, compose_opt (fn x => SOME x) (fn y => SOME y) 425)
	val _ = UnitTest.assertEquals_IntOption(SOME 4240, compose_opt (fn x => SOME (x-10)) (fn y => SOME (y*10)) 425)
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("do_until")
	val _ = UnitTest.assertEquals_Int(3, do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 12)
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("factorial")
	val _ = UnitTest.assertEquals_Int(1, factorial 1)
	val _ = UnitTest.assertEquals_Int(2, factorial 2)
	val _ = UnitTest.assertEquals_Int(6, factorial 3)
	val _ = UnitTest.assertEquals_Int(24, factorial 4)
	val _ = UnitTest.assertEquals_Int(120, factorial 5)
	val _ = UnitTest.assertEquals_Int(720, factorial 6)
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("fixed_point")
	val _ = UnitTest.assertEquals_Int(425, fixed_point (fn x => 425 ) 425)
	val _ = UnitTest.assertEquals_Int(4, fixed_point (fn x => ( x div 2 ) + 2 ) 16)
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("map2")
	val _ = UnitTest.assertEquals_IntInt((16,25), map2 (fn x => x*x ) (4,5) ) 
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("app_all")
	val _ = 
		let
			fun f n = [n, 2*n, 3*n]
		in
			UnitTest.assertEquals_IntList([1,2,3, 2,4,6, 3,6,9], app_all f f 1 ) 
		end
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("foldr")
	val _ = UnitTest.assertEquals_Int(10, foldr (fn(acc, x)=>(acc+x)) 0 [1,2,3,4])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("partition")
	val _ = UnitTest.assertEquals_IntListIntList(([],[]), partition (fn x => true) [])
	val _ = UnitTest.assertEquals_IntListIntList(([],[]), partition (fn x => false) [])
	val _ = UnitTest.assertEquals_IntListIntList(([425],[]), partition (fn x => true) [425])
	val _ = UnitTest.assertEquals_IntListIntList(([],[425]), partition (fn x => false) [425])
	val _ = UnitTest.assertEquals_IntListIntList(([425],[231]), partition (fn x => x>300) [231,425])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("unfold")
	val _ = UnitTest.assertEquals_IntList([5,4,3,2,1], unfold (fn n=>if n=0 then NONE else SOME(n,n-1)) 5) 
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("map")
	val _ = 
		let
			val xs = [1,2,3,4,5,6,7,8]
			fun square x =
				x*x
		in
			UnitTest.assertEquals_IntList(List.map square xs, map square xs)
		end
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("filter")
	val _ = 
		let
			val xs = [1,2,3,4,5,6,7,8]
			fun isEven x =
				(x mod 2) = 0
		in
			UnitTest.assertEquals_IntList(List.filter isEven xs, filter isEven xs)
		end
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("foldl")
	val _ = 
		let
			val xs = [1,2,3,4,5,6,7,8]
			fun f( x, acc ) =
				(x*x)::acc
		in
			UnitTest.assertEquals_IntList(List.foldl f [] xs, foldl f [] xs)
		end
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("mapTree")
	val _ = assertEquals_IntTree(NODE(9,NODE(16,LEAF,LEAF),NODE(25,LEAF,LEAF)), (mapTree (fn x => x*x) (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)))))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("foldTree")
	val _ = UnitTest.assertEquals_Int(12, foldTree (fn (y,acc) => y+acc) 0 (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF))))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("filterTree")
	val _ = assertEquals_IntTree(LEAF, (filterTree (fn x => false) (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)))))
	val _ = assertEquals_IntTree(NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)), (filterTree (fn x => true) (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)))))
	val _ = assertEquals_IntTree(NODE(3,NODE(4,LEAF,LEAF),LEAF), (filterTree (fn x => x<5) (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)))))
	val _ = assertEquals_IntTree(NODE(3,LEAF,NODE(5,LEAF,LEAF)), (filterTree (fn x => x<>4) (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)))))
	val _ = assertEquals_IntTree(NODE(3,LEAF,LEAF), (filterTree (fn x => x=3) (NODE(3,NODE(4,LEAF,LEAF),NODE(5,LEAF,LEAF)))))
	val _ = UnitTest.leave()

	val _ = print "<<< end test error output\n\n"
in
	()
end


