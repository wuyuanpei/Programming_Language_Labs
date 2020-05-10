(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Richard Wu *)

fun only_capitals sl = 
	List.filter (fn x => Char.isUpper(String.sub(x,0))) sl

fun longest_string1 sl =
	List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" sl

fun longest_string2 sl =
	List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" sl

fun longest_string_helper f = 
	List.foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) ""

fun longest_string3 sl =
	let
		val func = longest_string_helper (fn (x,y) => x > y) 
	in
		func sl
	end

fun longest_string4 sl =
	let
		val func = longest_string_helper (fn (x,y) => x >= y)
	in
		func sl
	end

fun longest_capitalized sl =
	let
		val func = longest_string1 o only_capitals
	in
		func sl
	end

fun rev_string s = 
	(String.implode o List.rev o String.explode) s

fun first_answer f al =
	case al of
		[] => raise NoAnswer
		| a::b => case f a of
					SOME v => v
					| NONE => first_answer f b

fun all_answers f al =
	let
		fun all_answers_help al acc = 
			case al of
				[] => SOME acc
				| a::b => case f a of
							NONE => NONE
							| SOME v => all_answers_help b (acc@v)		
	in
		all_answers_help al []
	end

(* 9(a): g takes two functions f1, f2, and a pattern p (Notice that they are in currying form)
   Based on the functions and the pattern, g computes an integer as its output *)

fun count_wildcards p = 
	g (fn _=> 1) (fn _=> 0) p

fun count_wild_and_variable_lengths p =
	g (fn _=> 1) (fn x=> String.size x) p

fun count_some_var (s, p) =
	g (fn _=> 0) (fn x=> if x=s then 1 else 0) p

fun check_pat p =
	let
		fun extract_strs (p,acc) =
			case p of
				Variable x => x::acc
				| TupleP ps => List.foldl extract_strs acc ps
				| ConstructorP(_,p) => extract_strs (p,acc)
				| _ => acc

		fun no_repeat ss =
			case ss of
				[] => true
				| a::b => (not (List.exists (fn x=> x=a) b)) andalso (no_repeat b)
	in
		no_repeat (extract_strs (p, []))
	end

fun match (v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
	   |(v, Variable s) => SOME [(s,v)]
	   |(Unit, UnitP) => SOME []
	   |(Const n, ConstP m) => if n=m then SOME [] else NONE
	   |(Tuple vs, TupleP ps) => if (List.length vs) = (List.length ps) then all_answers match (ListPair.zip(vs, ps)) else NONE
	   |(Constructor (s2, v), ConstructorP(s1, p)) => if s1=s2 then match(v, p) else NONE
	   | _ => NONE

fun first_match v ps =
	SOME (first_answer (fn p => match(v,p)) ps)
	handle NoAnswer => NONE
