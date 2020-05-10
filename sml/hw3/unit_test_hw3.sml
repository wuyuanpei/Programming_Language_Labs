use "../unit_test/unit_test.sml";
use "hw3.sml";

val _ = print "\n\n\n>>> begin test error output\n"

val _ = let
    fun assertEqualsOneOf (to_repr) ((expecteds : ''a list), (actual : ''a)) =
		let 
			fun equals_one_of(expecteds : ''a list) : bool =
			case expecteds of
				[] => false
				| expected::expecteds' => 
					if expected = actual
					then true
					else equals_one_of(expecteds')
			val to_string = (Repr.toString o to_repr)
			val list_to_string = (Repr.toString o (Repr.listToRepr to_repr) ) 
		in
			if equals_one_of(expecteds)
			then UnitTest.success( "equals one of: " ^ to_string(actual))
			else UnitTest.failure( "expected one of: " ^ list_to_string(expecteds) ^ "; actual: " ^ to_string(actual) )
		end

	val assertEqualsOneOf_IntListOption = assertEqualsOneOf (Repr.optToRepr (Repr.listToRepr Repr.I))

	fun valu_to_string(v : valu) =
		case v of
		  Const(n) => "Const(" ^ Int.toString(n) ^ ")"
		| Unit => "Unit"
	    | Tuple(list) => "Tuple(" ^ 
			let
				val value_list_to_string = (Repr.toString o (Repr.listToRepr valu_to_repr)) 
			in
				value_list_to_string(list)
			end 
		^ ")"
	    | Constructor(s,u) => "Constructor(\"" ^ s ^ "," ^ valu_to_string(v) ^ "\")"

	and valu_to_repr(v : valu) =
		Repr.S(valu_to_string(v))
	
	val assertEquals_StringValuListOption = UnitTest.assertEquals (Repr.toString o (Repr.optToRepr (Repr.listToRepr (Repr.t2ToRepr Repr.S valu_to_repr))))

	val _ = UnitTest.enter("only_capitals")
	val _ = UnitTest.assertEquals_StringList(["A","B","C"], only_capitals ["A","B","C"])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("longest_string1")
	val _ = UnitTest.assertEquals_String("bc", longest_string1 ["A","bc","C"])
	val _ = UnitTest.assertEquals_String("early", longest_string1 ["early","later"])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("longest_string2")
	val _ = UnitTest.assertEquals_String("bc", longest_string2 ["A","bc","C"])
	val _ = UnitTest.assertEquals_String("later", longest_string2 ["early","later"])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("longest_string3")
	val _ = UnitTest.assertEquals_String("bc", longest_string3 ["A","bc","C"])
	val _ = UnitTest.assertEquals_String("early", longest_string3 ["early","later"])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("longest_string4")
	val _ = UnitTest.assertEquals_String("C", longest_string4 ["A","B","C"])
	val _ = UnitTest.assertEquals_String("later", longest_string4 ["early","later"])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("longest_capitalized")
	val _ = UnitTest.assertEquals_String("A", longest_capitalized ["A","bc","C"])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("rev_string")
	val _ = UnitTest.assertEquals_String("cba", rev_string "abc")
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("first_answer")
	val _ = UnitTest.assertEquals_Int(4, first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("all_answers")
	val _ = UnitTest.assertEquals_IntListOption(NONE, all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7])
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("all_answers_ascending")
	val all_answers_ascending = [2,20,3,30,4,40,5,50,6,60,7,70]
	val all_answers_descending = [7,70,6,60,5,50,4,40,3,30,2,20] (* do not make the mistake of just calling List.rev on all_answers_ascending *)

	val _ = assertEqualsOneOf_IntListOption([SOME all_answers_ascending, SOME all_answers_descending], all_answers (fn x => SOME [x,x*10]) [2,3,4,5,6,7])

	val _ = UnitTest.leave()

	val _ = UnitTest.enter("count_wildcards")
	val _ = UnitTest.assertEquals_Int(1, count_wildcards Wildcard)
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("count_wild_and_variable_lengths")
	val _ = UnitTest.assertEquals_Int(1, count_wild_and_variable_lengths (Variable("a")))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("count_some_var")
	val _ = UnitTest.assertEquals_Int(1, count_some_var ("x", Variable("x")))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("check_pat")
	val _ = UnitTest.assertTrue(check_pat (Variable("x")))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("match")
	val _ = assertEquals_StringValuListOption(NONE, match (Const(1), UnitP))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("first_match")
	val _ = assertEquals_StringValuListOption(SOME [], first_match Unit [UnitP])
	val _ = UnitTest.leave()
in
	()
end

val _ = print "<<< end test error output\n\n"
