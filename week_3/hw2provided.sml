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

fun first_name {first=x, middle=y, last=z} = x

fun middle_name {first=x, middle=y, last=z} = y

fun last_name {first=x, middle=y, last=z} = z

fun string_not_present(s : string, lst : string list) =
  if null_func lst then true
  else if same_string(hd_func lst, s) then false
  else string_not_present(s, tl_func lst)

fun reverse_list xs =
  let
      fun aux(xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x::acc)
  in
      aux(xs, [])
  end

fun all_except_option(s : string, lst : string list) =
  let val stringNotPresent = string_not_present(s, lst)
  in
      if null_func lst orelse stringNotPresent then NONE
      else if same_string(hd_func lst, s) then SOME(tl_func lst)
      else let val remList = all_except_option(s, tl_func lst)
	   in
	       if isSome_func remList then SOME((hd_func lst)::(valOf_func remList))
	       else
		   SOME((hd_func lst)::[])
	   end
  end

fun get_substitutions1(lst : string list list, s : string) =
  if null_func lst then []
  else let val ansList = all_except_option(s, hd_func lst)
       in
	   if isSome_func ansList then ((valOf_func ansList) @ get_substitutions1(tl_func lst, s))
	   else
	       get_substitutions1(tl_func lst, s)
       end

fun get_substitutions2(lst : string list list, s : string) =
  if null_func lst then []
  else let val ansList = all_except_option(s, hd_func lst)
       in
	   if isSome_func ansList then ((valOf_func ansList) @ get_substitutions2(tl_func lst, s))
	   else
	       get_substitutions2(tl_func lst, s)
       end

fun similar_names(lst : string list list, name : {first:string, middle:string, last:string}) =
  if null_func lst then [name]
  else
      let
	  fun make_record_list(lst : string list, mname : string, lname : string, recordList : {first:string, middle:string, last:string} list) =
	    if null_func lst then recordList
	    else
		make_record_list(tl_func lst, mname, lname, ({first=(hd_func lst), middle=mname, last=lname}::recordList))
      in
	  make_record_list(reverse_list((first_name name)::get_substitutions1(lst, (first_name name))), middle_name name, last_name name, [])
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
