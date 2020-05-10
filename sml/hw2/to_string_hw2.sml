(* Dennis Cosgrove *)
use "../repr/repr.sml";
use "hw2.sml";

fun full_name_to_string(a : { first : string, middle : string, last : string }) =
	#first a ^ " " ^ #middle a ^ " " ^ #last a

fun full_name_to_repr(a : { first : string, middle : string, last : string }) =
	Repr.S(full_name_to_string(a))

val full_name_list_to_string = (Repr.toString o (Repr.listToRepr full_name_to_repr))

fun card_to_string(s : suit, r : rank) =
	let
		fun suit_to_string(s : suit) =
			case s of
			  Spades => "Spades"
			| Clubs => "Clubs"
			| Diamonds => "Diamonds"
			| Hearts => "Hearts"
		fun rank_to_string(r : rank) =
			case r of
			  Num v => "Num " ^ Int.toString(v)
			| Jack => "Jack"
			| Queen => "Queen"
			| King => "King"
			| Ace => "Ace"
	in
		"(" ^ suit_to_string(s) ^ "," ^ rank_to_string(r) ^ ")"
	end

fun card_to_repr(c : card) =
	Repr.S(card_to_string(c))

val card_list_to_string = (Repr.toString o (Repr.listToRepr card_to_repr))

fun color_to_string(c : color) =
	case c of
	  Black => "Black"
	| Red => "Red"

fun move_to_repr(m : move) =
	Repr.S(
		case m of
			Draw => "Draw"
		| Discard(c) => "Discard(" ^ card_to_string(c) ^ ")"
	)
val move_list_to_string = (Repr.toString o (Repr.listToRepr move_to_repr))
