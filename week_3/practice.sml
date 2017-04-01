(* Some of the things that are not allowed to use in Homework 2

1. isSome
2. valOf
3. hd
4. tl
5. null
6. #

*)

(* Records, Tuples are special case of records *)

val x = {bar = (1 + 2, true andalso true), foo = 3 + 4, baz = (false, 9)}
val first_pair = (1, 2)
val second_pair = {1=2, 2=3}
val record = {1=2, 3=2}

(* Datatypes *)

datatype mytype = TwoInts of int * int | Str of string | Pizza

(* Accessing datatypes values *)

val a = Str("Hi")
val b = Str
val c = Pizza
val d = TwoInts(1+2, 3+4)
val e = a

(* Use datatypes as parameter in function then do the patter matching *)

fun f(x : mytype) =
  case x of
      Pizza => 3
    | Str s => 8
    | TwoInts(i1, i2) => i1 + i2

fun g x = case x of Pizza => 3 (* Warning: match nonexhaustive *)

datatype id = StudentNum of int
	    | Name of string * string option * string

(* Expression trees or Recursion *)

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

fun eval e =
  case e of
      Constant i => i
   |  Negate i => ~ (eval i)
   |  Add (i1, i2) => (eval i1) + (eval i2)
   |  Multiply (i1, i2) => (eval i1) * (eval i2)

fun eval2 (Constant i) = i
  | eval2 (Negate i) = ~(eval2 i)
  | eval2 (Add (i1, i2)) = (eval2 i1) + (eval2 i2)
  | eval2 (Multiply (i1, i2)) = (eval2 i1) * (eval2 i2)

fun max_constant e =
  case e of
      Constant e1 => e1
    | Negate e1 => max_constant e1
    | Add (e1, e2) => Int.max(max_constant e1, max_constant e2)
    | Multiply (e1, e2) => Int.max(max_constant e1, max_constant e2)

val test_exp = Add(Constant(19), Negate(Constant(4)))
val nineteen = max_constant test_exp

(* Type synonyms *)

type name_record = {Student_num : int option,
		    first : string,
		    middle : string option,
		    last : string}

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Ace | King | Queen | Jack | Num of int
type card = suit * rank

fun is_queen_of_spades(c : card) =
  #1 c = Spade andalso #2 c = Queen

fun is_queen_of_spades2 c =
  case c of
      (Spade, Queen) => true
    | _ => false

fun sum_list xs =
  case xs of
      [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
      [] => ys
    | x::xs' => x :: append(xs', ys)

fun append2 ([], ys) = ys
  | append2 (x::xs', ys) = x :: append2(xs', ys)

fun inc_or_zero intoption =
  case intoption of
      NONE => 0
    | SOME i => i + 1

(* Use these instead of isSome, valOf, hd, tl, null and # *)

fun isSomeFunction intoption =
  case intoption of
      NONE => false
    | SOME i => true

fun valOfFunction intoption =
  case intoption of
      NONE => raise Option
    | SOME i => i

fun hdFunction xs =
  case xs of
      [] => raise List.Empty
    | x::xs' => x

fun tlFunction xs =
  case xs of
      [] => raise List.Empty
    | x::xs' => xs'

fun nullFunction xs =
  case xs of
      [] => true
    | x::xs' => false

fun sum_triple triple =
  case triple of
      (x, y, z) => x + y + z

fun sum_triple2 (x, y, z) =
  x + y + z

fun full_name name =
  case name of
      {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun full_name2 {first=x, middle=y, last=z} =
  x ^ " " ^ y ^ " " ^ z

(* Polymorphic and equality types *)

type foo = int * int

(* This is more general
{quxx : 'b, bar : int * 'a, baz : 'b}
then
(quxx : string, bar : foo, baz : string)
which is equivalent to
{quxx : string, bar : int * int, baz : string}
*)
(* Variables with two quotes
''a list * ''a -> bool
These are equality types that arise from = operator
 *)

(* Nested Pattern Matching *)

exception ListLengthException
fun zip (l1, l2, l3) =
  case l1 of
      [] =>
      (case l2 of
	  [] =>
	  (case l3 of
	      [] => []
	    | _ => raise ListLengthException)
        | _ => raise ListLengthException)
    | hd1::tl1 =>
      (case l2 of
	   [] => raise ListLengthException
	 | hd2::tl2 =>
	   (case l3 of
		[] => raise ListLengthException
	      | hd3::tl3 => (hd1, hd2, hd3)::zip(tl1, tl2, tl3)))

fun zip2 list_triple =
  case list_triple of
      ([],[],[]) => []
    | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip(tl1, tl2, tl3)
    | _ => raise ListLengthException

fun unzip lst =
  case lst of
      [] => ([], [], [])
    | (a, b, c)::tl => let val (l1, l2, l3) = unzip tl
		       in
			   (a::l1, b::l2, c::l3)
		       end

fun nondecreasing xs =
  case xs of
      [] => true
    | _::[] => true
    | head::(neck::xs') => head <= neck andalso nondecreasing(neck:: xs')

datatype sgn = N | P | Z

fun multisgn (x1, x2) =
  let
      fun sign x =
	if x = 0 then Z
	else if x > 0 then P
	else N
  in
      case (sign x1, sign x2) of
	  (_, Z) => Z
        | (Z, _) => Z
	| (P, P) => P
	| (N, N) => P
	| _ => N
  end

(* Tail recursion *)

fun revList xs =
  case xs of
      [] => []
    | x::xs' => revList xs' @ [x]

fun revListTailRecursion xs =
  let
      fun aux(xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x::acc)
  in
      aux(xs, [])
  end
