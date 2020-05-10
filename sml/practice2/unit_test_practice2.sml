use "../unit_test/unit_test.sml";
use "practice2.sml";

val _ = let
	val _ = print "\n\n\n>>> begin test error output\n"

	fun pass_fail_to_string(pf : pass_fail) =
		case pf of
		  pass => "pass"
		| fail => "fail"

	val assertEquals_PassFail = UnitTest.assertEquals pass_fail_to_string

	fun assertPass(actual : pass_fail) = 
		assertEquals_PassFail(pass, actual)

	fun assertFail(actual : pass_fail) = 
		assertEquals_PassFail(fail, actual)


	fun flag_to_string(f : flag) : string =
		case f of
		leave_me_alone => "leave_me_alone"
		| prune_me => "prune_me"
	fun flag_tree_to_string(t : flag tree) : string =
		case t of
		leaf => "leaf"
		| node {value=v,left=lt,right=rt} => "node {value=" ^ flag_to_string(v) ^ ",left=" ^ flag_tree_to_string(lt) ^ ",right=" ^ flag_tree_to_string(rt) ^ ";"

	val assertEquals_FlagTree = UnitTest.assertEquals flag_tree_to_string

	fun nat_to_string(n : nat) =
		case n of
		  ZERO => "ZERO"
		| SUCC n' => "SUCC " ^ nat_to_string(n')

	val assertEquals_Nat = UnitTest.assertEquals nat_to_string
		
	val _ = UnitTest.enter("pass_or_fail")
	val _ = assertPass(pass_or_fail ({grade=SOME 76, id=0}))
	val _ = assertPass(pass_or_fail ({grade=SOME 75, id=0}))
	val _ = assertFail(pass_or_fail ({grade=SOME 74, id=0}))
	val _ = assertFail(pass_or_fail ({grade=NONE, id=0}))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("has_passed")
	val _ = UnitTest.assertTrue(has_passed ({grade=SOME 76, id=0}))
	val _ = UnitTest.assertTrue(has_passed ({grade=SOME 75, id=0}))
	val _ = UnitTest.assertFalse(has_passed ({grade=SOME 74, id=0}))
	val _ = UnitTest.assertFalse(has_passed ({grade=NONE, id=0}))
	val _ = UnitTest.leave()

	fun create_final_grade(g : int) : final_grade =
		{grade=SOME g, id=0}

	fun create_final_grades(gs : int list) : final_grade list =
		let 
			fun aux(gs : int list, acc : final_grade list) = 
				case gs of
				[] => acc
				| g :: gs' => aux(gs', create_final_grade(g)::acc)
		in
			aux(gs, [])
		end

	val _ = UnitTest.enter("number_passed")
	val _ = UnitTest.assertEquals_Int(0, number_passed (create_final_grades([])))
	val _ = UnitTest.assertEquals_Int(0, number_passed (create_final_grades([70])))
	val _ = UnitTest.assertEquals_Int(1, number_passed (create_final_grades([80])))
	val _ = UnitTest.assertEquals_Int(2, number_passed (create_final_grades([50, 80, 60, 90, 70])))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("number_misgraded")
	val _ = UnitTest.assertEquals_Int(0, number_misgraded( []))
	val _ = UnitTest.assertEquals_Int(0, number_misgraded( [(pass,create_final_grade(90))]))
	val _ = UnitTest.assertEquals_Int(1, number_misgraded( [(pass,create_final_grade(50))]))
	val _ = UnitTest.assertEquals_Int(0, number_misgraded( [(fail,create_final_grade(50))]))
	val _ = UnitTest.assertEquals_Int(1, number_misgraded( [(fail,create_final_grade(90))]))
	val _ = UnitTest.assertEquals_Int(2, number_misgraded( [(pass,create_final_grade(50)), (fail,create_final_grade(90))]))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("tree_height")
	val _ = UnitTest.assertEquals_Int(0, tree_height(leaf))
	val _ = UnitTest.assertEquals_Int(1, tree_height(node {value=231, left=leaf, right=leaf}))
	val _ = UnitTest.assertEquals_Int(2, tree_height(node {value=231, left=leaf, right=node {value=425, left=leaf, right=leaf}}))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("sum_tree")
	val _ = UnitTest.assertEquals_Int(0, sum_tree(leaf))
	val _ = UnitTest.assertEquals_Int(231, sum_tree(node {value=231, left=leaf, right=leaf}))
	val _ = UnitTest.assertEquals_Int(231+425, sum_tree(node {value=231, left=leaf, right=node {value=425, left=leaf, right=leaf}}))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("gardener")
	val _ = assertEquals_FlagTree(leaf, gardener(leaf))
	val treeA = node {value=leave_me_alone, left=leaf, right=leaf}
	val _ = assertEquals_FlagTree(treeA, gardener(treeA))
	val treeB = node {value=leave_me_alone, left=leaf, right=node {value=prune_me, left=leaf, right=leaf}}
	val _ = assertEquals_FlagTree(treeA, gardener(treeB))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("my_last")
	val _ = ( my_last([]) ; raise UnitTestBase.AssertFailure("expected Empty exception"))
			handle Empty => UnitTest.success("my_last([]) throws Empty exception")
	fun my_last_test_all(vss) = 
		case vss of
		[] => ()
		| vs::vss' => ( UnitTest.assertEquals_Int
	(List.last(vs), my_last(vs)); my_last_test_all(vss'))
	val _ = my_last_test_all([[1], [1,2], [1,2,3], [1,2,3,4]])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("my_take")
	val _ = ( my_take([], ~1) ; raise UnitTestBase.AssertFailure("expected Subscript exception"))
			handle Subscript => UnitTest.success("my_take([], ~1) throws Subscript exception")
	val _ = ( my_take([1,2,3,4], 5) ; raise UnitTestBase.AssertFailure("expected Subscript exception"))
			handle Subscript => UnitTest.success("[1,2,3,4], 5) throws Subscript exception")
	fun my_take_test_all(vsis) = 
		case vsis of
		[] => ()
		| (vs,i)::vsis' => ( UnitTest.assertEquals_IntList(List.take(vs,i), my_take(vs,i)); my_take_test_all(vsis'))
	val _ = my_take_test_all([([1,2,3,4],0), ([1,2,3,4],1), ([1,2,3,4],2), ([1,2,3,4],3), ([1,2,3,4],4)])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("my_drop")
	val _ = ( my_drop([], ~1) ; raise UnitTestBase.AssertFailure("expected Subscript exception"))
			handle Subscript => UnitTest.success("my_drop([], ~1) throws Subscript exception")
	val _ = ( my_drop([1,2,3,4], 5) ; raise UnitTestBase.AssertFailure("expected Subscript exception"))
			handle Subscript => UnitTest.success("[1,2,3,4], 5) throws Subscript exception")
	fun my_drop_test_all(vsis) = 
		case vsis of
		[] => ()
		| (vs,i)::vsis' => ( UnitTest.assertEquals_IntList(List.drop(vs,i), my_drop(vs,i)); my_drop_test_all(vsis'))
	val _ = my_drop_test_all([([1,2,3,4],0), ([1,2,3,4],1), ([1,2,3,4],2), ([1,2,3,4],3), ([1,2,3,4],4)])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("my_concat")
	fun my_concat_test_all(vsss) = 
		case vsss of
		[] => ()
		| vss::vsss' => ( UnitTest.assertEquals_IntList(List.concat(vss), my_concat(vss)); my_concat_test_all(vsss'))
	val _ = my_concat_test_all([
		[], 
		[[],[],[]],
		[[1]], 
		[[1], [2,3], [4,5,6]]
	])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("my_getOpt")
	val _ = UnitTest.assertEquals_Int(425, my_getOpt(NONE, 425))
	val _ = UnitTest.assertEquals_Int(231, my_getOpt(SOME 231, 425))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("my_join")
	fun my_join_test_all(opt_opts) = 
		case opt_opts of
		[] => ()
		| opt_opt::opt_opts' => ( UnitTest.assertEquals_IntOption(Option.join(opt_opt), my_join(opt_opt)); my_join_test_all(opt_opts'))
	val _ = my_join_test_all([NONE, SOME(NONE), SOME(SOME(2))])
	val _ = UnitTest.leave()

	val one = SUCC ZERO
	val two = SUCC one
	val three = SUCC two
	val four = SUCC three
	val five = SUCC four
	val six = SUCC five

	val _ = UnitTest.enter("is_positive")
	val _ = UnitTest.assertFalse(is_positive ZERO)
	val _ = UnitTest.assertTrue(is_positive one)
	val _ = UnitTest.assertTrue(is_positive two)
	val _ = UnitTest.assertTrue(is_positive three)
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("pred")
	val _ = assertEquals_Nat(two, pred three)
	val _ = assertEquals_Nat(one, pred two)
	val _ = assertEquals_Nat(ZERO, pred one)
	val _ = ( pred(ZERO) ; raise UnitTestBase.AssertFailure("expected Negative exception") )
			handle Negative => UnitTest.success("pred(ZERO) throws Negative exception") 
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("nat_to_int")
	val _ = UnitTest.assertEquals_Int(0, nat_to_int(ZERO))
	val _ = UnitTest.assertEquals_Int(1, nat_to_int(one))
	val _ = UnitTest.assertEquals_Int(2, nat_to_int(two))
	val _ = UnitTest.assertEquals_Int(3, nat_to_int(three))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("int_to_nat")
	val _ = assertEquals_Nat(ZERO, int_to_nat(0))
	val _ = assertEquals_Nat(one, int_to_nat(1))
	val _ = assertEquals_Nat(two, int_to_nat(2))
	val _ = assertEquals_Nat(three, int_to_nat(3))
	val _ = ( int_to_nat(~1) ; raise UnitTestBase.AssertFailure("expected Negative exception") )
			handle Negative => UnitTest.success("int_to_nat(~1) throws Negative exception") 
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("add")
	val _ = assertEquals_Nat(ZERO, add(ZERO, ZERO))
	val _ = assertEquals_Nat(one, add(ZERO, one))
	val _ = assertEquals_Nat(one, add(one, ZERO))
	val _ = assertEquals_Nat(two, add(one, one))
	val _ = assertEquals_Nat(three, add(two, one))
	val _ = assertEquals_Nat(three, add(one, two))
	val _ = assertEquals_Nat(four, add(two, two))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("sub")
	val _ = assertEquals_Nat(ZERO, sub(ZERO, ZERO))

	val _ = assertEquals_Nat(one, sub(one, ZERO))
	val _ = assertEquals_Nat(ZERO, sub(one, one))

	val _ = assertEquals_Nat(two, sub(two, ZERO))
	val _ = assertEquals_Nat(one, sub(two, one))
	val _ = assertEquals_Nat(ZERO, sub(two, two))

	val _ = assertEquals_Nat(three, sub(three, ZERO))
	val _ = assertEquals_Nat(two, sub(three, one))
	val _ = assertEquals_Nat(one, sub(three, two))
	val _ = assertEquals_Nat(ZERO, sub(three, three))

	val _ = assertEquals_Nat(four, sub(four, ZERO))
	val _ = assertEquals_Nat(three, sub(four, one))
	val _ = assertEquals_Nat(two, sub(four, two))
	val _ = assertEquals_Nat(one, sub(four, three))
	val _ = assertEquals_Nat(ZERO, sub(four, four))

	val _ = ( sub(one, two) ; raise UnitTestBase.AssertFailure("expected Negative exception") )
			handle Negative => UnitTest.success("sub(one, two) throws Negative exception") 

	val _ = UnitTest.leave()

	val _ = UnitTest.enter("mult")
	val _ = assertEquals_Nat(ZERO, mult(ZERO, ZERO))
	val _ = assertEquals_Nat(ZERO, mult(three, ZERO))
	val _ = assertEquals_Nat(ZERO, mult(ZERO, three))
	val _ = assertEquals_Nat(two, mult(one, two))
	val _ = assertEquals_Nat(four, mult(four, one))
	val _ = assertEquals_Nat(four, mult(two, two))
	val _ = assertEquals_Nat(six, mult(three, two))
	val _ = assertEquals_Nat(six, mult(two, three))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("less_than")
	val _ = UnitTest.assertTrue(less_than(ZERO, one))
	val _ = UnitTest.assertTrue(less_than(ZERO, two))
	val _ = UnitTest.assertTrue(less_than(ZERO, six))
	val _ = UnitTest.assertFalse(less_than(ZERO, ZERO))
	val _ = UnitTest.assertFalse(less_than(one, ZERO))
	val _ = UnitTest.assertFalse(less_than(two, ZERO))
	val _ = UnitTest.assertFalse(less_than(six, ZERO))
	val _ = UnitTest.assertTrue(less_than(one, three))
	val _ = UnitTest.assertTrue(less_than(one, two))
	val _ = UnitTest.assertTrue(less_than(one, six))
	val _ = UnitTest.assertFalse(less_than(three, one))
	val _ = UnitTest.assertFalse(less_than(two, one))
	val _ = UnitTest.assertFalse(less_than(six, one))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("isEmpty")
	val _ = UnitTest.assertTrue(isEmpty(Elems []))
	val _ = UnitTest.assertFalse(isEmpty(Elems [425]))
	val _ = UnitTest.assertTrue(isEmpty(Union(Elems [], Elems [])))
	val _ = UnitTest.assertFalse(isEmpty(Union(Elems [425], Elems [])))
	val _ = UnitTest.assertFalse(isEmpty(Union(Elems [], Elems [425])))
	val _ = UnitTest.assertFalse(isEmpty(Union(Elems [425], Elems [231])))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("contains")
	val _ = UnitTest.assertFalse(contains(Elems [], 425))
	val _ = UnitTest.assertTrue(contains(Elems [425], 425))
	val _ = UnitTest.assertFalse(contains(Elems [425], 231))
	val _ = UnitTest.assertTrue(contains(Elems [231, 425], 425))
	val _ = UnitTest.assertTrue(contains(Elems [231, 425], 231))
	val _ = UnitTest.assertTrue(contains(Range {from=231, to=425}, 300))
	val _ = UnitTest.assertFalse(contains(Range {from=231, to=425}, 100))
	val _ = UnitTest.assertFalse(contains(Range {from=231, to=425}, 500))
	val _ = UnitTest.assertTrue(contains(Union(Elems([231, 425]), Elems([4, 66, 99])), 4))
	val _ = UnitTest.assertTrue(contains(Union(Elems([231, 425]), Elems([4, 66, 99])), 66))
	val _ = UnitTest.assertTrue(contains(Union(Elems([231, 425]), Elems([4, 66, 99])), 99))
	val _ = UnitTest.assertTrue(contains(Union(Elems([231, 425]), Elems([4, 66, 99])), 231))
	val _ = UnitTest.assertTrue(contains(Union(Elems([231, 425]), Elems([4, 66, 99])), 425))
	val _ = UnitTest.assertFalse(contains(Union(Elems([231, 425]), Elems([4, 66, 99])), 100))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([231, 425]), Elems([4, 66, 99])), 4))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([231, 425]), Elems([4, 66, 99])), 66))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([231, 425]), Elems([4, 66, 99])), 99))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([231, 425]), Elems([4, 66, 99])), 231))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([231, 425]), Elems([4, 66, 99])), 425))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([231, 425]), Elems([4, 66, 99])), 100))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([1, 2, 3, 4]), Range{from=3, to=7}), 2))
	val _ = UnitTest.assertTrue(contains(Intersection(Elems([1, 2, 3, 4]), Range{from=3, to=7}), 3))
	val _ = UnitTest.assertTrue(contains(Intersection(Elems([1, 2, 3, 4]), Range{from=3, to=7}), 4))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([1, 2, 3, 4]), Range{from=3, to=7}), 5))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([1, 2, 3, 4]), Range{from=3, to=7}), 6))
	val _ = UnitTest.assertFalse(contains(Intersection(Elems([1, 2, 3, 4]), Range{from=3, to=7}), 7))
	val _ = UnitTest.leave()

	(* TODO: test in any order *)
	val _ = UnitTest.enter("toList")
	val _ = UnitTest.assertEquals_IntList([], toList(Elems []))
	val _ = UnitTest.assertEquals_IntList([425], toList(Elems [425]))
	val _ = UnitTest.assertEquals_IntList([231,425], toList(Elems [231, 425]))
	val _ = UnitTest.assertEquals_IntList([231,425], toList(Union(Elems [231], Elems([425]))))
	val _ = UnitTest.assertEquals_IntList([4,12], toList(Intersection(Elems [2,4,6,8,10,12], Elems([4,9,12,33]))))
	val _ = UnitTest.assertEquals_IntList([4,12], toList(Intersection(Union(Elems [4,6,10], Elems [2,8,12]), Elems([4,9,12,33]))))
	val _ = UnitTest.assertEquals_IntList([10,12], toList(Intersection(Elems [2,4,6,8,10,12], Range{from=9, to=33})))
	val _ = UnitTest.leave()

	val _ = print "<<< end test error output\n\n"
in
	()
end