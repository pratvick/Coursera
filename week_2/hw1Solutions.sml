fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 = #1 date2 then if #2 date1 = #2 date2 then #3 date1 < #3 date2
			      else #2 date1 < #2 date2
  else #1 date1 < #1 date2

fun number_in_month(datesList : (int * int * int) list, month : int) =
  if null datesList then 0
  else let
      val numOfMonthsInRemList = number_in_month(tl datesList, month)
  in
      if (#2 (hd datesList)) = month then 1 + numOfMonthsInRemList
      else numOfMonthsInRemList
  end

fun number_in_months(datesList : (int * int * int) list, monthsList : int list) =
  if null monthsList then 0
  else number_in_month(datesList, hd monthsList) + number_in_months(datesList, tl monthsList)
      
