fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 = #1 date2 then if #2 date1 = #2 date2 then #3 date1 < #3 date2
			      else #2 date1 < #2 date2
  else #1 date1 < #1 date2

fun number_in_month(datesList : (int * int * int) list, month : int) =
  if null datesList then 0
  else if (#2 (hd datesList)) = month then 1 + number_in_month(tl datesList, month)
  else number_in_month(tl datesList, month)

fun number_in_months(datesList : (int * int * int) list, monthsList : int list) =
  if null monthsList then 0
  else number_in_month(datesList, hd monthsList) + number_in_months(datesList, tl monthsList)

fun dates_in_month(datesList : (int * int * int) list, month : int) =
  if null datesList then []
  else if (#2 (hd datesList)) = month then (hd datesList) :: dates_in_month(tl datesList, month)
  else dates_in_month(tl datesList, month)

fun dates_in_months(datesList : (int * int * int) list, monthsList: int list) =
  if null monthsList then []
  else dates_in_month(datesList, hd monthsList) @ dates_in_months(datesList, tl monthsList)

fun get_nth(items : string list, index : int) =
  if index = 1 then hd items
  else get_nth(tl items, index - 1)

fun date_to_string(date : int * int * int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, numbers : int list) =
  if (sum - (hd numbers)) <= 0 then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

fun what_month(day : int) =
  let
      val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
      1 + number_before_reaching_sum(day, days)
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  if null dates then NONE
  else let
      fun oldest_non_empty(dates : (int * int * int) list) =
	if null (tl dates) then hd dates
	else let
	    val oldestDate = oldest_non_empty(tl dates)
	in
	    if is_older(oldestDate, hd dates) then oldestDate
	    else hd dates
	end
  in
      SOME (oldest_non_empty dates)
  end
