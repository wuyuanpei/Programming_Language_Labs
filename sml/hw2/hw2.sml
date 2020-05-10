(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* Richard Wu *)
fun all_except_option(s: string, ss: string list) = 
    let
      fun helper(ss: string list, acc: string list) = 
        case ss of
        [] => NONE
        | a::b => if same_string(a, s) then SOME (acc@b) else helper(b,a::acc)
    in
      helper(ss, [])
    end

fun get_substitutions1(sss: string list list, s: string) = 
    case sss of
    [] => []
    | a::b => case all_except_option(s, a) of
                NONE => get_substitutions1(b, s)
                | SOME ss  => ss @ get_substitutions1(b, s)

fun get_substitutions2(sss: string list list, s: string) = 
    let
      fun helper(sss: string list list, acc: string list) = 
        case sss of
        [] => acc
        | a::b => case all_except_option(s, a) of
                NONE => helper(b, acc)
                | SOME ss  => helper(b, acc @ ss)
    in
      helper(sss, [])
    end
    
fun similar_names(sss: string list list, {first=x,middle=y,last=z}) = 
    let
      fun helper(ls: string list, acc: {first:string,middle:string,last:string} list) = 
        case ls of
        [] => acc
        | a::b => helper(b, {first=a,middle=y,last=z}::acc)
    in
      {first=x,middle=y,last=z}::helper(get_substitutions2(sss, x),[])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* Richard Wu *)

fun card_color(c: card) = 
    case c of 
    (Clubs,_) => Black
    | (Spades,_) => Black
    | (Hearts,_) => Red
    | (Diamonds,_) => Red

fun card_value(c: card) = 
    case c of 
    (_,Num n) => n
    | (_,Ace) => 11
    | _ => 10

fun remove_card(cs: card list, c: card, e: exn) = 
    let
      fun helper (cs: card list, acc: card list) =
      case cs of
        [] => raise e
        | a::b => if a = c then acc@b else helper(b, a::acc)
    in
      helper(cs, [])
    end

fun all_same_color(cs: card list) =
    case cs of
      [] => true
      | a::[] => true
      | a::(b::c) => card_color(a) = card_color(b) andalso all_same_color(b::c)
    
fun sum_cards(cs: card list) =
    let
      fun helper(cs: card list, acc: int) = 
        case cs of
          [] => acc
          | a::b => helper(b, acc + card_value(a))
    in
      helper(cs, 0)
    end

fun score(cs: card list, goal: int) =
  let
    val sum = sum_cards(cs)
    val preliminary = 
      if sum > goal
      then 3 * (sum - goal)
      else goal - sum
  in
    if all_same_color(cs)
    then preliminary div 2
    else preliminary
  end

fun officiate(cs: card list, ms: move list, goal: int) =
  let
    fun helper(cs: card list, ms: move list, hl: card list) =
      case ms of
      [] => score(hl, goal)
      | (Discard c)::b => helper(cs, b, remove_card(hl, c, IllegalMove))
      | Draw::b => case cs of
                    [] => score(hl, goal)
                    | x::y => if (card_value(x) + sum_cards(hl)) > goal
                              then score(x::hl, goal)
                              else helper(y, b, x::hl)
  in
    helper(cs, ms, [])
  end

fun score_challenge(cs: card list, goal: int) =
  let
    fun each_add(int_list: int list, num: int) = 
        case int_list of
          [] => []
          | a::b => (a+num)::each_add(b,num)

    fun sum_helper(cs: card list, res: int list) = 
        case cs of
          [] => res
          | (_,Ace)::b => sum_helper(b, each_add(res, 1)@each_add(res, 11))
          | a::b => sum_helper(b, each_add(res, card_value(a)))
    
    val sum_P = sum_helper(cs,[0])
    
    fun each_score(sum: int) =
    let
      val preliminary = 
      if sum > goal
      then 3 * (sum - goal)
      else goal - sum
    in
      if all_same_color(cs)
      then preliminary div 2
      else preliminary
    end

    fun min_score(sum_P: int list)=
      case sum_P of
        a::[] => each_score(a)
        | a::b => 
                  let 
                    val min = min_score(b)
                    val a_score = each_score(a)
                  in
                    if a_score > min
                    then min
                    else a_score
                  end
  in
    min_score(sum_P)
  end


fun officiate_challenge(cs: card list, ms: move list, goal: int) =
  let
    fun sum_cards(cs: card list) =
    let
      fun helper(cs: card list, acc: int) = 
        case cs of
          [] => acc
          | (_,Ace)::b => helper(b, acc + 1)
          | a::b => helper(b, acc + card_value(a))
    in
      helper(cs, 0)
    end
    fun helper(cs: card list, ms: move list, hl: card list) =
      case ms of
      [] => score_challenge(hl, goal)
      | (Discard c)::b => helper(cs, b, remove_card(hl, c, IllegalMove))
      | Draw::b => case cs of
                    [] => score_challenge(hl, goal)
                    | (s,Ace)::y => if (1 + sum_cards(hl)) > goal
                              then score_challenge((s,Ace)::hl, goal)
                              else helper(y, b, (s,Ace)::hl)
                    | x::y => if (card_value(x) + sum_cards(hl)) > goal
                              then score_challenge(x::hl, goal)
                              else helper(y, b, x::hl)
  in
    helper(cs, ms, [])
  end

  fun careful_player(cs: card list, goal: int) =
    let
      fun search_discard(hl: card list, value: int)=
        case hl of
          [] => NONE
          | a::b => if card_value(a) = value 
                    then SOME a
                    else search_discard(b, value)
      fun helper(cs: card list, ms: move list, hl: card list) =
        if score(hl, goal) = 0
        then ms
        else
          case cs of
            [] => ms
            |a::b =>
              if sum_cards(hl) < goal - 10
              then helper(b, ms@[Draw], a::hl)
              else 
              case search_discard(hl, sum_cards(hl) + card_value(a) - goal) of
                NONE => ms
              | SOME c => ms@((Discard c)::[Draw])
            
    in
      helper(cs,[],[])
    end
