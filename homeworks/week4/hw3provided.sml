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

val only_capitals = List.filter (Char.isUpper o (fn s => String.sub(s, 0)))


val longest_string1 = 
  let 
    fun longer_string(x, y) = if String.size x > String.size y then x else y
  in List.foldl longer_string ""
  end


val longest_string2 = 
  let 
    fun longer_string(x, y) = if String.size x >= String.size y then x else y
  in List.foldl longer_string ""
  end


fun longest_string_helper f sl = 
  let 
    fun longer_string(x, y) = if f(String.size x, String.size y) then x else y
  in List.foldl longer_string "" sl
  end


val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


val longest_capitalized = longest_string3 o only_capitals


val rev_string = String.implode o List.rev o String.explode


fun first_answer f ls =
  case ls of 
       [] => raise NoAnswer
     | x::xs => case (f x) of
                     SOME v => v
                   | NONE => first_answer f xs


fun all_answers f ls =
  let fun aux(acc, ls) =
    case ls of
         [] => SOME acc
       | x::xs => case (f x) of
                       NONE => NONE
                     | SOME v => aux(acc@v, xs)
  in aux([], ls)
  end



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


val count_wildcards = g (fn () => 1) (fn (_) => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)
fun count_some_var(str, p) =
  g (fn (_) => 0) (fn x => if str = x then 1 else 0) p


fun check_pat p = 
  let 
    fun extract_var(p, acc) =
      case p of
           Variable x => acc @ [x]
         | TupleP ps  => List.foldl extract_var acc ps
         | ConstructorP(_, p') => extract_var(p', acc)
         | _          => acc
    fun is_repeat sl = 
      case sl of
           []    => true
         | x::xs => if (List.exists (fn y => (y=x)) xs)
                    then false
                    else is_repeat xs
  in is_repeat(extract_var(p, []))
  end


fun match(v, p) =
  case (v, p) of
       (x, Wildcard)       => SOME []
     | (_, Variable s)     => SOME [(s, v)]
     | (Unit, UnitP)       =>  SOME []
     | (Const i, ConstP j) => if i = j then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                then all_answers match (ListPair.zip(vs, ps))
                                else NONE
     | (Constructor(s1, v'), ConstructorP(s2, p')) => 
         if String.size s1 = String.size s2
         then match(v', p')
         else NONE
     | _ => NONE


fun first_match v ps =
  SOME (first_answer (fn p => match(v, p)) ps) 
  handle NoAnswer => NONE
