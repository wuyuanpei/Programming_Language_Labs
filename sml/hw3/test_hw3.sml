(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["A","bcd","Csd"] = "bcd"
val test22 = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 ["A","bcd","Csd"] = "Csd"
val test32 = longest_string2 [] = ""

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 ["A","bcd","Csd"] = "bcd"
val test4a2 = longest_string3 [] = ""

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 ["A","bcd","Csd"] = "Csd"
val test4b2 = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized ["d","A231","bc2323","Ccfe"] = "A231"
val test52 = longest_capitalized ["re","bc","ecvec"] = ""
val test53 = longest_capitalized ["A","bcew","e"] = "A"

val test6 = rev_string "abc" = "cba"
val test61 = rev_string "1234567" = "7654321"
val test62 = rev_string "" = ""
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x > 3 then SOME [x,1] else NONE) [2,3,4,5,6,7] = NONE
val test82 = all_answers (fn x => if x > 1 then SOME [x,1] else NONE) [2,3,4,5,6,7] = SOME [2,1,3,1,4,1,5,1,6,1,7,1]
val test83 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test84 = all_answers (fn x => if x > 3 andalso x < 6 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test85 = all_answers (fn x => if true then SOME [x+1] else NONE) [2,3,4,5,6,7] = SOME [3,4,5,6,7,8]
val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (Variable "fgfg") = 0
val test9a2 = count_wildcards (TupleP [Wildcard, Variable "", Wildcard, Wildcard]) = 3
val test9a3 = count_wildcards (TupleP [Wildcard, Variable "", TupleP [Wildcard, Wildcard, Wildcard], Wildcard]) = 5
val test9a4 = count_wildcards (TupleP [Wildcard, Variable "", ConstructorP ("ddvc", Wildcard), Wildcard]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (Variable("awdfe")) = 5
val test9b2 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "", Wildcard, Wildcard]) = 3
val test9b3 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "rjrknv", Wildcard, Wildcard]) = 9
val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("x", TupleP [Variable("x"),Wildcard,Variable("x")]) = 2

val test10 = check_pat (Variable("x"))
val test101 = check_pat (Wildcard)
val test102 = check_pat (ConstructorP ("sfd",Variable("y")))
val test103 = check_pat (TupleP [Variable("ffw"), Variable("34j"), Wildcard, ConstructorP ("sfd", Variable("ffw"))]) = false
val test104 = check_pat (TupleP [Variable("ffw"), Variable("ffw")]) = false



val test11 = match (Const(1), UnitP) = NONE
val test111 = match (Unit, UnitP) = SOME []
val test112 = match (Const(11), ConstP 11) = SOME []
val test113 = match (Const(1), Wildcard) = SOME []
val test114 = match (Const(1), Variable ("hhh")) = SOME [("hhh",Const 1)]
val test115 = match (Constructor("dfsa",Unit), ConstructorP("df",Variable("f"))) = NONE
val test116 = match (Constructor("dfsa",Unit), ConstructorP("dfsa",Variable("f"))) = SOME [("f",Unit)]
val test117 = match (Tuple([Const 1, Unit]),TupleP([Variable ("a"),Variable ("b")])) = SOME [("a",Const 1),("b",Unit)]
val test12 = first_match Unit [UnitP,Variable "a"] = SOME []
val test121 = first_match Unit [Variable "a",UnitP] = SOME [("a",Unit)]
val test122 = first_match (Const 6) [ConstP 3,UnitP] = NONE
val test123 = first_match (Tuple([Const 1, Unit])) [TupleP([Variable ("a"),Variable ("b")])] = SOME [("a",Const 1),("b",Unit)]