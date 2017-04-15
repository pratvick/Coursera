fun only_capitals xs =
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
  List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs =
  List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs =
  List.foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) "" xs

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
  let fun helper (ys, acc) =
     case ys of
          [] => acc
        | y::ys' => case f y of
                         NONE => []
                       | SOME(x) => helper(ys', x @ acc)
   in
     case helper(xs, []) of
          [] => NONE
        | ys => SOME(ys)
   end

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

fun count_wildcards p =
  g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
  g (fn () => 1) (fn x => String.size(x)) p

fun count_some_var (x, p) =
  g (fn () => 0) (fn y => if y = x then 1 else 0) p

fun helper_1 p =
  case p of
      Wildcard          => []
    | Variable x        => [x]
    | TupleP ps         => List.foldl (fn (p,i) => i @ (helper_1 p)) [] ps
    | ConstructorP(_,p) => helper_1 p
    | _                 => []

fun helper_2 xs =
  case xs of
      [] => true
    | x::xs' => if List.exists (fn y => x = y) xs' then false else helper_2 xs'

fun check_pat p =
  (helper_2 o helper_1) p

fun match x =
  case x of
      (_,Wildcard) => SOME []
    | (v,Variable x) => SOME [(x,v)]
    | (Unit, UnitP) => SOME []
    | (Const c,ConstP y) => if y = c then SOME [] else NONE
    | (Tuple vs, TupleP ps) => all_answers (match) (ListPair.zip(vs,ps))
    | (Constructor(s1,v),ConstructorP(s2,p)) => if s2 = s1 then (match (v, p)) else NONE
    | (_,_) => NONE

fun first_match v ps =
  SOME (first_answer (fn x => match(v,x)) (ps))
  handle NoAnswer => NONE
