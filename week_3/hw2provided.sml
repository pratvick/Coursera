fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun isSome_func intoption =
  case intoption of
      NONE => false
    | SOME i => true

fun valOf_func intoption =
  case intoption of
      NONE => raise Option
    | SOME i => i

fun hd_func xs =
  case xs of
      [] => raise List.Empty
    | x::xs' => x

fun tl_func xs =
  case xs of
      [] => raise List.Empty
    | x::xs' => xs'

fun null_func xs =
  case xs of
      [] => true
    | x::xs' => false

fun all_except_option(s : string, lst : string list) =
  case lst of
      [] => NONE
    | head::tail => if same_string(s, head)
                then SOME tail
                else case all_except_option(s, tail) of
                         NONE => NONE
                       | SOME y => SOME(head :: y)

fun get_substitutions1(lst : string list list, s : string) =
  case lst of
      [] => []
    | head::tail => case all_except_option(s, head) of
			NONE => get_substitutions1(tail, s)
		      | SOME y => y @ get_substitutions1(tail, s)

fun get_substitutions2(lst : string list list, s : string) =
  let
      fun aux (acc, lst_left) =
	case lst_left of
	    [] => acc
	  | head::tail => aux ((case all_except_option(s, head) of
                                NONE => acc
                              | SOME y => acc @ y),
                               tail)
  in
      aux([], lst)
  end

fun similar_names(lst : string list list, name : {first:string, middle:string, last:string}) =
  let
      val {first=f, middle=m, last=l} = name
      fun make_names xs =
	case xs of
	    [] => []
	  | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
  in
      name::make_names(get_substitutions2(lst, f))
  end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color c =
  case c of
      (Spades, _) => Black
    | (Clubs, _) => Black
    | _ => Red

fun card_value c =
  case c of
      (_, Ace) => 11
    | (_, Num i) => i
    | _ => 10

fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | c1::cs' => if c1 = c then cs'
		 else
		     c1::remove_card(cs', c, e)

fun all_same_color cs =
  case cs of
      [] => true
    | _::[] => true
    | head::(neck::cs') => (card_color head) = (card_color neck) andalso all_same_color(neck::cs')

fun sum_cards cs =
  let
      fun aux(cs, sum) =
	case cs of
	    [] => sum
	  | c::cs' => aux(cs', sum + card_value c)
  in
      aux(cs, 0)
  end

fun score (cs, goal) =
  let fun prelim_score(cs, goal) =
	let val sumCards = sum_cards cs
	in
	    if sumCards < goal then goal - sumCards
	    else
		3 * (sumCards - goal)
	end
  in
      if all_same_color cs then prelim_score(cs, goal) div 2
      else prelim_score(cs, goal)
  end

fun officiate(clst : card list, mlst : move list, goal : int) =
  let fun play(clst, mlst, hlst) =
	case mlst of
	    [] => score(hlst, goal)
	  | (Discard c)::mlst' => play(clst, mlst', remove_card(hlst, c, IllegalMove))
	  | Draw::mlst' => if null_func clst then score(hlst, goal)
			   else if sum_cards((hd_func clst)::hlst) > goal then score((hd_func clst)::hlst, goal)
			   else play(tl_func clst, mlst', (hd_func clst)::hlst)
  in
      play(clst, mlst, [])
  end
