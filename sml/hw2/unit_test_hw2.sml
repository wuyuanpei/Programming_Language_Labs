use "../unit_test/unit_test.sml";
use "hw2.sml";

val _ = let
	val _ = print "\n\n\n>>> begin test error output\n"

	fun full_name_to_repr(a : { first : string, middle : string, last : string }) =
		Repr.S(#first a ^ " " ^ #middle a ^ " " ^ #last a)

	val assertEquals_FullNameList = UnitTest.assertEquals (Repr.toString o (Repr.listToRepr full_name_to_repr))

	fun color_to_string(c : color) =
		case c of
		  Black => "Black"
		| Red => "Red"

	val assertEquals_Color = UnitTest.assertEquals color_to_string

	fun card_to_repr(c : card) =
		let
			fun suit_to_string(s : suit) =
				case s of
				  Spades => "Spades"
				| Clubs => "Clubs"
				| Diamonds => "Diamonds"
				| Hearts => "Hearts"
			fun rank_to_string(r : rank) =
				case r of
				  Num v => Int.toString(v)
				| Jack => "Jack"
				| Queen => "Queen"
				| King => "King"
				| Ace => "Ace"
			val (s, r) = c
		in
			Repr.S("(" ^ suit_to_string(s) ^ "," ^ rank_to_string(r) ^ ")")
		end

	val assertEquals_CardList = UnitTest.assertEquals (Repr.toString o (Repr.listToRepr card_to_repr))

	val _ = UnitTest.enter("all_except_option")
	val _ = UnitTest.assertEquals_StringListOption(SOME [], all_except_option ("string", ["string"]))
	val _ = UnitTest.assertEquals_StringListOption(SOME ["athos", "aramis", "porthos"], all_except_option("d'artagnan", ["d'artagnan", "athos", "aramis", "porthos"]))
	val _ = UnitTest.assertEquals_StringListOption(SOME ["d'artagnan", "aramis", "porthos"], all_except_option("athos", ["d'artagnan", "athos", "aramis", "porthos"]))
	val _ = UnitTest.assertEquals_StringListOption(SOME ["d'artagnan", "athos", "aramis"], all_except_option("porthos", ["d'artagnan", "athos", "aramis", "porthos"]))
	val _ = UnitTest.assertEquals_StringListOption(NONE, all_except_option("d'artagnan", ["athos", "aramis", "porthos"]))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("get_substitutions1")
	val _ = UnitTest.assertEquals_StringList([], get_substitutions1 ([["foo"],["there"]], "foo"))
	val _ = UnitTest.assertEquals_StringList(["Fredrick","Freddie","F"], get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"))
	val _ = UnitTest.assertEquals_StringList(["Jeffrey","Geoff","Jeffrey"], get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff"))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("get_substitutions2")
	val _ = UnitTest.assertEquals_StringList([], get_substitutions2 ([["foo"],["there"]], "foo"))
	val _ = UnitTest.assertEquals_StringList(["Fredrick","Freddie","F"], get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"))
	val _ = UnitTest.assertEquals_StringList(["Jeffrey","Geoff","Jeffrey"], get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff"))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("similar_names")
	val similar_names_a = assertEquals_FullNameList([
		{first="Fred", last="Smith", middle="W"}, 
		{first="Fredrick", last="Smith", middle="W"},
		{first="Freddie", last="Smith", middle="W"}, 
		{first="F", last="Smith", middle="W"}], 
		similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("card_color")
	val _ = assertEquals_Color(Black, card_color (Clubs, Num 2))
	val _ = assertEquals_Color(Red, card_color (Diamonds, Jack))
	val _ = assertEquals_Color(Red, card_color (Hearts, Queen))
	val _ = assertEquals_Color(Black, card_color (Spades, Ace))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("card_value")
	val _ = UnitTest.assertEquals_Int(2, card_value (Clubs, Num 2))
	val _ = UnitTest.assertEquals_Int(3, card_value (Diamonds, Num 3))
	val _ = UnitTest.assertEquals_Int(4, card_value (Hearts, Num 4))
	val _ = UnitTest.assertEquals_Int(5, card_value (Spades, Num 5))
	val _ = UnitTest.assertEquals_Int(6, card_value (Clubs, Num 6))
	val _ = UnitTest.assertEquals_Int(7, card_value (Diamonds, Num 7))
	val _ = UnitTest.assertEquals_Int(8, card_value (Hearts, Num 8))
	val _ = UnitTest.assertEquals_Int(9, card_value (Spades, Num 9))
	val _ = UnitTest.assertEquals_Int(10, card_value (Clubs, Num 10))
	val _ = UnitTest.assertEquals_Int(10, card_value (Diamonds, Jack))
	val _ = UnitTest.assertEquals_Int(10, card_value (Hearts, Queen))
	val _ = UnitTest.assertEquals_Int(10, card_value (Spades, King))
	val _ = UnitTest.assertEquals_Int(11, card_value (Spades, Ace))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("remove_card")
	val _ = assertEquals_CardList([], remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove))
	val _ = assertEquals_CardList([(Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], remove_card ([(Clubs, Num 2), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], (Clubs, Num 2), IllegalMove))
	val _ = assertEquals_CardList([(Clubs, Num 2), (Hearts, Queen), (Spades, Ace)], remove_card ([(Clubs, Num 2), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], (Diamonds, Jack), IllegalMove))
	val _ = assertEquals_CardList([(Clubs, Num 2), (Diamonds, Jack), (Spades, Ace)], remove_card ([(Clubs, Num 2), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], (Hearts, Queen), IllegalMove))
	val _ = assertEquals_CardList([(Clubs, Num 2), (Diamonds, Jack), (Hearts, Queen)], remove_card ([(Clubs, Num 2), (Diamonds, Jack), (Hearts, Queen), (Spades, Ace)], (Spades, Ace), IllegalMove))
	val _ = assertEquals_CardList([(Hearts, Jack), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], remove_card ([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], (Hearts, Num 10), IllegalMove))
	val _ = assertEquals_CardList([(Hearts, Num 10), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], remove_card ([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], (Hearts, Jack), IllegalMove))
	val _ = assertEquals_CardList([(Hearts, Num 10), (Hearts, Jack), (Hearts, King), (Hearts, Ace)], remove_card ([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], (Hearts, Queen), IllegalMove))
	val _ = assertEquals_CardList([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, Ace)], remove_card ([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], (Hearts, King), IllegalMove))
	val _ = assertEquals_CardList([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, King)], remove_card ([(Hearts, Num 10), (Hearts, Jack), (Hearts, Queen), (Hearts, King), (Hearts, Ace)], (Hearts, Ace), IllegalMove))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("all_same_color")
	val _ = UnitTest.assertTrue(all_same_color([(Hearts, Ace), (Hearts, Ace)]))
	val _ = UnitTest.assertTrue(all_same_color([]))
	val _ = UnitTest.assertTrue(all_same_color([(Clubs, King)]))
	val _ = UnitTest.assertTrue(all_same_color([(Clubs, King), (Clubs, Ace)]))
	val _ = UnitTest.assertTrue(all_same_color([(Clubs, King), (Spades, Ace)]))
	val _ = UnitTest.assertFalse(all_same_color([(Hearts, King), (Spades, Ace)]))
	val _ = UnitTest.assertFalse(all_same_color([(Hearts, King), (Hearts, King), (Spades, King)]))
	val _ = UnitTest.assertFalse(all_same_color([(Hearts, King), (Spades, King), (Hearts, King)]))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("sum_cards")
	val _ = UnitTest.assertEquals_Int(4, sum_cards([(Clubs, Num 2),(Clubs, Num 2)]))
	val _ = UnitTest.assertEquals_Int(0, sum_cards([]))
	val _ = UnitTest.assertEquals_Int(2, sum_cards([(Clubs, Num 2)]))
	val _ = UnitTest.assertEquals_Int(10, sum_cards([(Clubs, Jack)]))
	val _ = UnitTest.assertEquals_Int(12, sum_cards([(Clubs, Num 2), (Clubs, Jack)]))
	val _ = UnitTest.assertEquals_Int(21, sum_cards([(Hearts, Ace), (Hearts, King)]))
	val _ = UnitTest.leave()

	val _ = UnitTest.enter("score")
	val _ = UnitTest.assertEquals_Int(4, score([(Hearts, Num 2),(Clubs, Num 4)],10))
	val _ = UnitTest.leave()

	val _ = print "<<< end test error output\n\n"
in
	()
end

