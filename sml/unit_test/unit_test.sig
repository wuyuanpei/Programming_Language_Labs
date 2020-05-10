(* TODO: use CM.make to avoid having to go up and down a directory??? *)
use "../unit_test/unit_test_base.sig";

signature UNIT_TEST = sig include UNIT_TEST_BASE
    val assertEquals_Int : (int*int) -> unit
    val assertEquals_IntList : (int list * int list) -> unit
    val assertEquals_IntOption : (int option * int option) -> unit
    val assertEquals_IntListOption : (int list option * int list option) -> unit

    val assertEquals_IntInt : ((int*int) * (int*int)) -> unit
    val assertEquals_IntIntList : (((int*int) list) * ((int*int) list)) -> unit
    val assertEquals_IntIntOption : (((int*int) option) * ((int*int) option)) -> unit
    val assertEquals_IntIntListOption : (((int*int) list option) * ((int*int) list option)) -> unit

    val assertEquals_IntIntInt : ((int*int*int) * (int*int*int)) -> unit
    val assertEquals_IntIntIntList : (((int*int*int) list) * ((int*int*int) list)) -> unit
    val assertEquals_IntIntIntOption : (((int*int*int) option) * ((int*int*int) option)) -> unit
    val assertEquals_IntIntIntListOption : (((int*int*int) list option) * ((int*int*int) list option)) -> unit

    val assertEquals_IntListIntList : ((int list*int list) * (int list*int list)) -> unit

    val assertEquals_String : (string*string) -> unit
    val assertEquals_StringList : (string list * string list) -> unit
    val assertEquals_StringOption : (string option * string option) -> unit
    val assertEquals_StringListOption : (string list option * string list option) -> unit

    val assertEqualsForwardOrReverse_IntList : (int list * int list) -> unit
    val assertEqualsAnyOrder_IntList : (int list * int list) -> unit

    val assertEqualsForwardOrReverse_StringList : (string list * string list) -> unit
    val assertEqualsAnyOrder_StringList : (string list * string list) -> unit
end
