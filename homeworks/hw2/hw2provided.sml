(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* put your solutions for problem 1 here *)
fun all_except_option(str, ls) =
  let fun aux(ls, acc) =
    case ls of 
         [] => NONE
       | x::xs => if same_string(str, x)
                  then SOME (acc @ xs)
                  else aux(xs, x::acc)
  in aux(ls, [])
  end


fun get_substitutions1(lss, s) = 
  case lss of 
     [] => []
   | x::xs => case all_except_option(s, x) of
                   NONE => get_substitutions1(xs, s)
                 | SOME some_ls => some_ls @ get_substitutions1(xs, s)


fun get_substitutions2(lss, s) = 
  let fun aux(lss, acc) = 
    case lss of 
         [] => acc
       | x::xs => case all_except_option(s, x) of
                       NONE => aux(xs, acc)
                     | SOME some_ls => aux(xs, acc @ some_ls)
  in aux(lss, [])
  end


fun similar_names(lss, full_name) =
  let 
    val {first=x, middle=y, last=z} = full_name
    val first_names = get_substitutions2(lss, x)
    fun aux(first_names) =
      case first_names of 
           [] => []
         | n::ns => {first=n, middle=y, last=z}::aux(ns)
  in
  full_name::aux(first_names)
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

fun card_color(card) = 
  case card of
     (Clubs, _) => Black
   | (Spades, _) => Black
   | (_, _) => Red


fun card_value(card) = 
  case card of
     (_, Num i) => i
   | (_, Ace) => 11
   | (_, _) => 10


fun remove_card(cs, c, e) = 
  let fun aux(cs, acc) = 
    case cs of 
         [] => raise e
       | x::xs => if c = x
                  then xs @ acc
                  else aux(xs, x::acc)
  in 
      aux(cs, [])
  end


fun all_same_color(cs) = 
  case cs of
       [] => true
     | _::[] => true
     | c1::c2::rest => (card_color(c1) = card_color(c2))
             andalso all_same_color(c2::rest)


fun sum_cards(cs) =
  let fun aux(cs, acc) =
    case cs of 
         [] => acc
       | x::xs => aux(xs, card_value(x)+acc)
  in aux(cs, 0)
  end


fun score(cards, goal) =
  let 
    val sum = sum_cards(cards)
    val pre_score = if sum > goal
                    then (sum - goal) * 3
                    else goal - sum
  in if all_same_color cards
     then pre_score div 2
     else pre_score
  end


fun officiate(cards, moves, goal) =
  let fun aux(cards, moves, helds) = 
    case moves of
        [] => score(helds, goal)
       | m::ms => case m of
                       Discard c => aux(cards, ms, remove_card(helds, c, IllegalMove))
                     | Draw =>
                         case cards of
                              [] => score(helds, goal)
                            | c::cs => 
                                let val new_helds = c::helds
                                in if sum_cards(new_helds) <= goal
                                   then aux(cs, ms, new_helds)
                                   else score(new_helds, goal)
                                end
  in aux(cards, moves, [])
  end


fun count_ace(cs) = 
  case cs of 
       [] => 0
     | (_, Ace)::xs => count_ace(xs) + 1
     | _::xs => count_ace(xs)

     
fun p_score(n, s, goal) =
  let 
    val pre_score = if s <= goal then goal - s else (s - goal) * 3
  in
    case n of 
         0 => pre_score
       | _ => Int.min(pre_score, p_score(n-1, s-10, goal))
  end


fun score_challenge(cards, goal) = 
  let val pre_score = p_score(count_ace cards, sum_cards cards, goal)
  in if all_same_color cards
     then pre_score div 2
     else pre_score
  end 


fun officiate_challenge(cards, moves, goal) =
  let fun aux(cards, moves, helds) = 
    case moves of
        [] => score_challenge(helds, goal)
       | m::ms => case m of
                       Discard c => aux(cards, ms, remove_card(helds, c, IllegalMove))
                     | Draw =>
                         case cards of
                              [] => score_challenge(helds, goal)
                            | c::cs => 
                                let 
                                  val new_helds = c::helds
                                  val n_ace = count_ace(new_helds)
                                in 
                                  if sum_cards(new_helds) - n_ace * 10 <= goal
                                  then aux(cs, ms, new_helds)
                                  else score_challenge(new_helds, goal)
                                end
  in aux(cards, moves, [])
  end


fun careful_player(cards, goal) = 
  let 
    fun aux(cards, helds, moves) = 
      let 
        val sum_held = sum_cards helds
        fun loop(card, helds, moves) = 
          case helds of
               [] => []
             | x::xs => if card_value(card) = card_value(x)
                        then [Discard x]
                        else loop(card, xs, moves)
      in
        case cards of 
             [] => 
               if goal - sum_held > 10
               then moves @ [Draw]
               else moves
           | c::cs => 
               if sum_held + card_value(c) = goal
               then moves @ [Draw]
               else if goal - sum_held > 10
               then aux(cs, c::helds, moves@[Draw])
               else
                 case loop(c, helds, moves) of
                      [] => if sum_held + card_value(c) > goal
                            then case helds of
                                      [] => moves
                                    | h::hs => aux(cards, hs, moves@[Discard h])
                            else aux(cs, c::helds, moves@[Draw])
                    | d::[] => moves @ [d, Draw]
      end
  in aux(cards, [], [])
  end


