fun sort_pair(pr1 : int * int, pr2 : int * int) =
  if (#1 pr1) < (#1 pr2) then (pr1, pr2)
  else if (#1 pr1) > (#1 pr2) then (pr2, pr1)
  else if (#2 pr1) < (#2 pr2) then (pr1, pr2)
  else (pr2, pr1)

fun countDown(a : int) =
  if a = 0 then []
  else a :: countDown(a - 1)
		     
fun append(l1 : int list, l2 : int list) =
  if null l1 then l2
  else hd l1 :: append(tl l1, l2)
			
fun max(l1 : int list) =
  if null l1 then 0
  else if null (tl l1) then hd l1
  else
      let
	  val p = max(tl l1)
      in
	  if hd l1 > p then hd l1
	  else p	   
      end

fun max1(l1 : int list) =
  if null l1 then NONE
  else
      let
	  val x = max1(tl l1)
      in
	  if isSome x andalso valOf x > hd l1 then x
	  else SOME (hd l1)
      end

fun max2(l1 : int list) =
  if null l1 then NONE
  else let
	  fun maxNonEmpty(l1 : int list) =
	    if null (tl l1) then hd l1
	    else let
		val x = maxNonEmpty(tl l1)
	    in
		if x > hd l1 then x
		else hd l1
	    end
      in
	  SOME(maxNonEmpty l1)
      end
	  
	       
	  
		       




	  
	   
      
      
	  
      
