fun only_capitals xs =
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
  List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs =
  List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs =
  case xs of
      [] => ""
    | x::xs' => case xs' of
		    [] => x
		  | y::xs'' => if f(String.size x, String.size y) then longest_string_helper f (x::xs'')
			       else
				   longest_string_helper f xs'

val longest_string3 = longest_string_helper (fn (x, y) => x >= y)

val longest_string4 = longest_string_helper (fn (x, y) => x > y)

fun longest_capitalized xs =
  (longest_string1 o only_capitals) xs

fun rev_string s =
  (String.implode o List.rev o String.explode) s

exception NoAnswer

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => if isSome(f x) then valOf(f x) else first_answer f xs'

fun all_answers f xs =
  case xs of
      [] => SOME []
    | x::xs' => if isSome(f x) andalso isSome((all_answers f xs')) then SOME(valOf(f x) @ valOf(all_answers f xs'))
		else
		    NONE

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
