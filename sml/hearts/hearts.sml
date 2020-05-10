datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
type player = card list

(* Richard Wu *)
exception NotYetImplemented

fun is_card_valid(c : card) : bool =
	case c of
	(_,Num x) => x>1 andalso x<11
	| _ => true

fun are_all_cards_valid(taken_cards : card list) : bool =
	let
	  fun helper(taken_cards : card list, acc: bool) : bool =
		case taken_cards of
		[] => acc
		| a::b => helper(b, is_card_valid(a) andalso acc)
	in
	  helper(taken_cards, true)
	end

fun card_score(c : card) : int =
	case c of
	(Spades, Queen) => 13
	| (Hearts, _) => 1
	| (Diamonds, Jack) => ~10
	| _ => 0

fun total_score_of_card_list(cards : card list) : int =
	let
	  fun helper(cards : card list, acc: int) : int =
		case cards of
		[] => acc
		| a::b => helper(b, card_score(a) + acc)
	in
	  helper(cards, 0)
	end

fun total_score_of_player_list(players : player list) : int =
	let
	  fun helper(players : player list, acc: int) : int =
		case players of
		[] => acc
		| a::b => helper(b, total_score_of_card_list(a) + acc)
	in
	  helper(players, 0)
	end

fun total_card_count_for_all_players(players : player list) : int =
	let
	  fun count (f, pl) = 
	  	case pl of
		[] => 0
		| a::b => f(a) + count(f,b)
	in
	  count(List.length, players)
	end

fun is_correct_total_of_cards(players : player list) : bool =
	total_card_count_for_all_players(players) = 52

fun is_shenanigans_detected(players : player list) =
	total_score_of_player_list(players) <> 16
