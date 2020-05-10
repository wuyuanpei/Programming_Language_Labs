(* TODO: use CM.make to avoid having to go up and down a directory??? *)
use "../unit_test/unit_test_base.sig";
use "../command_line_args/command_line_args.sml";

structure UnitTestBase :> UNIT_TEST_BASE = struct
    exception AssertFailure of string;

    val out_file_path_ref : string option ref = ref(NONE)
    val is_raise_on_failure_ref : bool ref = ref(true)

    fun getOutFilePath() =
        !out_file_path_ref
    fun setOutFilePath(out_file_path : string option) : unit =
        ( out_file_path_ref := out_file_path
        ; case out_file_path of
                  NONE => ()
          | SOME(path) => OS.FileSys.remove(path) handle OS.SysErr(s) => ()
    )

    fun isRaiseOnFailure() =
        !is_raise_on_failure_ref
    fun setRaiseOnFailure(is_raise_on_failure: bool) :  unit = 
        is_raise_on_failure_ref := is_raise_on_failure

    fun processCommandLineArgs() =
        ( setRaiseOnFailure(CommandLineArgs.getBoolOrDefault("raiseOnFailure", true))
        ; setOutFilePath(CommandLineArgs.getStringOption("outFilePath"))
        )

    fun output(s:string) : unit =
        case getOutFilePath() of
                NONE => print(s)
        | SOME(path) =>
            let 
                val ostream = TextIO.openAppend path
                val _ = TextIO.output (ostream, s) handle e => (TextIO.closeOut ostream; raise e)
                val _ = TextIO.closeOut ostream
            in 
                ()
            end



    fun success(detail : string) : unit =
        output( "    success: " ^ detail ^ "\n" )

    fun failure(detail : string) : unit = 
        (
        output( "--------\n!!!\n!!!\n!!! ASSERTION FAILURE: " ^ detail ^ "\n!!!\n!!!\n--------\n" ) 
        ; 
        if isRaiseOnFailure()
        then raise AssertFailure( detail )
        else ()
        )


    fun enter( detail : string ) : unit =
        output( "testing " ^ detail ^ " {\n" )

    fun leave() : unit =
        output( "}\n" )

    fun assertTrue(actual : bool) =
        if actual
        then success("true")
        else failure("expected: true; actual: false")

    fun assertFalse(actual : bool) =
        if actual
        then failure("expected: false; actual: true")
        else success("false")

    fun assertEquals (to_string : ''a -> string) ((expected : ''a), (actual : ''a)) =
        if expected = actual
        then success( "equals: " ^ to_string(actual))
        else failure( "expected: " ^ to_string(expected) ^ "; actual: " ^ to_string(actual) )

    fun assertEqualsForwardOrReverse (to_string : ''a list -> string) ((expected : ''a list), (actual : ''a list)) =
        if (expected = actual) orelse (expected = rev(actual))
        then success( "equals (forward or reverse): " ^ to_string(actual))
        else failure( "expected (forward or reverse): " ^ to_string(expected) ^ "; actual: " ^ to_string(actual) )

    fun assertEqualsAnyOrder (to_string : ''a list -> string) (compare : ((''a * ''a) -> bool)) ((expected : ''a list), (actual : ''a list)) =
        if (ListMergeSort.sort compare expected) = (ListMergeSort.sort compare actual)
        then success( "equals (any order): " ^ to_string(actual))
        else failure( "expected (any order): " ^ to_string(expected) ^ "; actual: " ^ to_string(actual) )

    fun assertWithinDelta( expected : real, actual : real, delta : real ) = 
        raise Fail("TODO")

    fun assertWithinEpsilon( expected : real, actual : real, epsilon : real ) = 
        raise Fail("TODO")
end
