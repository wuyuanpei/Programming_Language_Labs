(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1001 = all_except_option ("string", ["string","a","b","c"]) = SOME ["a","b","c"]
val test1002 = all_except_option ("stringx", ["string"]) = NONE
val test1003 = all_except_option ("stringx", ["string","a","b","c"]) = NONE
val test1004 = all_except_option ("stringx", []) = NONE
val test1005 = all_except_option ("a", ["string","a","b","c"]) = SOME ["string","b","c"]
val test1006 = all_except_option ("stringx", ["c","stringx","a"]) = SOME ["c","a"]
val test1007 = all_except_option ("stringx", ["stringx","a"]) = SOME ["a"]
val test1008 = all_except_option ("a", ["stringx","a"]) = SOME ["stringx"]
val test1009 = all_except_option ("d", ["a","b","c","d"]) = SOME ["c","b","a"]
val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test21 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test22 =get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")=["Jeffrey","Geoff","Jeffrey"]
val test23 = get_substitutions1 ([["there"]], "foo") = []
val test24 = get_substitutions1 ([["there"],["foo"]], "foo") = []
val test25 = get_substitutions1 ([["there"],["foo","a"]], "foo") = ["a"]
val test26 = get_substitutions1 ([["foo","a"]], "foo") = ["a"]
val test27 = get_substitutions1 ([["foo","a"],["b","foo"]], "foo") = ["a","b"]
val test28 = get_substitutions1 ([[]], "foo") = []
val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test51 = card_color (Spades, Queen) = Black
val test52 = card_color (Diamonds, Num 10) = Red
val test53 = card_color (Hearts, Jack) = Red
val test6 = card_value (Clubs, Num 2) = 2
val test61 = card_value (Clubs, Num 10) = 10
val test62 = card_value (Diamonds, Ace) = 11
val test63 = card_value (Hearts, King) = 10
val test64 = card_value (Spades, Queen) = 10
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test71 = (remove_card ([(Hearts, Num 2),(Hearts, Ace),(Clubs, Ace)], (Spades, Ace), IllegalMove) handle IllegalMove => []) = []
val test72 = remove_card ([(Hearts, Num 2),(Hearts, Ace),(Clubs, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Num 2),(Clubs, Ace)]
val test73 = remove_card ([(Hearts, Num 2),(Hearts, Ace),(Clubs, Ace),(Spades, King)], (Hearts, Ace), IllegalMove) = [(Hearts, Num 2),(Clubs, Ace),(Spades, King)]
val test74 = remove_card ([(Hearts, Num 2),(Hearts, Ace),(Hearts, Ace),(Clubs, Ace),(Spades, King)], (Hearts, Ace), IllegalMove) = [(Hearts, Num 2),(Hearts, Ace),(Clubs, Ace),(Spades, King)]
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test81 = all_same_color [(Hearts, Ace), (Diamonds, Ace),(Hearts,Num 4)] = true
val test82 = all_same_color [(Clubs, Queen),(Hearts, Ace), (Hearts, Ace),(Spades, Queen)] = false
val test83 = all_same_color [(Hearts, Ace)] = true
val test84 = all_same_color [] = true
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test91 = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13
val test92 = sum_cards [(Clubs, Num 2),(Clubs, Ace),(Hearts, King),(Diamonds, Queen)] = 33
val test93 = sum_cards [(Clubs, Num 8)] = 8
val test94 = sum_cards [] = 0
val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test101 = score ([(Hearts, Num 2),(Clubs, Num 4)],3) = 9
val test102 = score ([(Hearts, Num 2),(Clubs, Num 4),(Clubs, Ace)],10) = 21
val test103 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test104 = score ([(Clubs, Num 2),(Clubs, Num 4),(Clubs, Ace)],10) = 10
val test105 = score ([],10) = 5
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42) = 3
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
val test311 = score_challenge ([(Hearts, Num 2),(Clubs, Ace)],10) = 7
val test310 = score_challenge ([(Clubs, Ace)],10) = 1
val test312 = score_challenge ([(Hearts, King),(Hearts,King),(Clubs, Ace),(Diamonds,Ace),(Spades,Ace)],3) = 60
val test313 = score_challenge ([],10) = 5
val test314 = score_challenge ([(Hearts, King),(Hearts,King),(Clubs, Ace)],30) = 3

val test315 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test316 = officiate_challenge ([(Hearts,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],30) = 6
val test317 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
val test318 = officiate_challenge ([(Diamonds, Ace),(Clubs, Num 2), (Clubs, Num 3)],[Draw, Draw, Draw], 6) = 0

val test321 = careful_player ([(Hearts, Num 2),(Clubs, Num 4)], 15) = [Draw,Draw]
val test322 = careful_player ([(Hearts, Num 9),(Clubs, Num 9)], 15) = [Draw]
val test323 = careful_player ([(Hearts, Num 9),(Clubs, Num 3),(Diamonds,Num 3)], 20) = [Draw,Draw]
val test324 = careful_player ([(Hearts, Num 9),(Clubs, Num 3),(Diamonds,Num 3)], 10) = []
val test325 = careful_player ([(Hearts, Num 9),(Clubs, Num 3),(Diamonds,Num 3)], 200) = [Draw,Draw,Draw]
val test326 = careful_player ([(Hearts, Num 9),(Clubs, Num 3),(Diamonds,Num 3)], 11) = [Draw]
val test327 = careful_player ([], 11) = []
val test328 = careful_player ([(Hearts, Num 9),(Clubs, Ace)], 11) = [Draw,Discard (Hearts,Num 9),Draw]
val test329 = careful_player ([(Hearts, Ace)], 11) = [Draw]
(* val test330 = careful_player ([(Hearts, Num 9),(Hearts, Num 4),(Hearts, Num 9),(Clubs, Num 6)], 24)
val score330 = officiate([(Hearts, Num 9),(Hearts, Num 4),(Hearts, Num 9),(Clubs, Num 6)], test330, 24) = 0 *)