(* Higher order functions -> Taking other functions as arguments *)

fun double x = 2 * x
fun inc x = x + 1
val a_tuple = (double, inc, double(inc 7))
val fourteen = (#1 a_tuple) 7

fun n_times(f, n, x) =
  if n = 0 then x
  else
      f(n_times(f, n - 1, x))

fun addition(n, x) = n_times(inc, n, x) (* inc(inc(inc(inc 0))) if n = 4 *)
fun nth_tail(n, xs) = n_times(tl, n, xs)

(* Anonymous functions *)

fun better_addition(n, x) = n_times((fn x => x + 1), n, x)

(* Map and Filter *)

fun map (f, xs) =
  case xs of
      [] => []
    | x::xs' => (f x)::map(f, xs')

fun filter (f, xs) =
  case xs of
      [] => []
    | x::xs' => if f x then x::(filter(f, xs'))
		else filter(f, xs')

fun all_even_snd xs =
  filter((fn (_, v) => (v mod 2 = 0)), xs)

fun allShorterThan(s, xs) =
  let
      val i = String.size s
  in
      filter(fn x => String.size x < i, xs)
  end

(* Functions returning functions *)

fun double_or_triple f =
  if f 7 then fn x => 2 * x
  else fn x => 3 * x

val double = double_or_triple(fn x => (x - 3) = 4)
val nine = (double_or_triple(fn x => x = 42)) 3

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

fun true_of_all_constants(f, e) =
  case e of
      Constant i => f i
    | Negate e1 => true_of_all_constants(f, e1)
    | Add(e1, e2) => true_of_all_constants(f, e1) andalso true_of_all_constants(f, e2)
    | Multiply(e1, e2) => true_of_all_constants(f, e1) andalso true_of_all_constants(f, e2) 

(* Fold *)

fun fold(f, acc, xs) =
  case xs of
      [] => acc
    | x::xs' => fold(f, f(acc, x), xs')

fun f1 xs =
  fold ((fn (x,y) => x andalso y >= 0), true, xs)
fun f2 (xs, lo, hi) =
  fold ((fn (x,y) => x + (if y >= lo andalso y <= hi then 1 else 0)), 0, xs)
fun f3 (g, xs) =
  fold ((fn (x,y) => x andalso g y), true, xs)

(* Functions compositions *)

fun compose (f, g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i
val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

fun backup (f, g) =
  fn x => case f x of
	      NONE => g x
	    | SOME y => y

(* Currying *)

fun sorted3_tupled (x, y, z) = z >= y andalso y >= x
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x
val t1 = ((sorted3 7) 9) 11
fun sorted3 x y z = z >= y andalso y >= x
val t2 = sorted3 7 9 11

(* Partial application *)

fun fold f acc xs =
  case xs of
      [] => acc
    | x::xs' => fold f (f(acc, x)) xs'
val sum = fold (fn (x, y) => x + y) 0

fun range i j = if i < j then [] else i :: range (i+1) j
val countup = range 1

fun exists f xs =
  case xs of
      [] => false
    | x::xs' => f x orelse exists f xs'
val hasZero = exists (fn x => x = 0)

val incrementAll = List.map (fn x => x + 1)
val removeZeros = List.filter (fn x => x <> 0)
fun incrementAllElements xs = incrementAll xs

(* Tupled to Curried *)

fun curry f x y = f(x, y)
fun range (i,j) = if i < j then [] else i :: range(i+1, j)
val countup = curry range 1
