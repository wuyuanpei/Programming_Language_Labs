use "../unit_test/unit_test.sml";
use "hw3.sml";

(* Dennis Cosgrove *)
val _ = let
    val _ = print "\n\n\n>>> begin test error output\n"

	fun typ_to_string(t : typ) : string =
		case t of
		  Anything => "Anything"
		| UnitT => "UnitT"
		| IntT => "IntT"
	    | TupleT(list) => "TupleT(" ^ 
			let
				val typ_list_to_string = (Repr.toString o (Repr.listToRepr typ_to_repr)) 
			in
				typ_list_to_string(list)
			end 
		^ ")"
	    | Datatype(s) => "Datatype(\"" ^ s ^ "\")"

	and typ_to_repr(t : typ) =
		Repr.S(typ_to_string(t))
	
	val assertEquals_TypOption = UnitTest.assertEquals (Repr.toString o (Repr.optToRepr typ_to_repr))

	val _ = UnitTest.enter("typecheck_patterns")
	val _ = assertEquals_TypOption(
        SOME(TupleT[Anything,Anything]), 
        typecheck_patterns([],[TupleP[Variable("x"),Variable("y")], TupleP[Wildcard,Wildcard]]))
	val _ = assertEquals_TypOption(
        SOME(TupleT[Anything,TupleT[Anything,Anything]]), 
        typecheck_patterns([],[TupleP[Wildcard,Wildcard], TupleP[Wildcard,TupleP[Wildcard,Wildcard]]]))
	val _ = UnitTest.leave()
    val _ = print "<<< end test error output\n\n"
in
	()
end

