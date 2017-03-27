(* Test cases for is_older function *)

test("Test 1 for is_older function", assert_true(is_older((2011, 11, 2), (2016, 12, 4))));
test("Test 2 for is_older function", assert_true(is_older((2011, 11, 1), (2011, 12, 4))));
test("Test 3 for is_older function", assert_true(is_older((2011, 11, 1), (2011, 11, 4))));
test("Test 4 for is_older function", assert_false(is_older((2011, 12, 1), (2011, 11, 1))));
test("Test 5 for is_older function", assert_false(is_older((2011, 11, 1), (2011, 11, 1))));

(* Test cases for number_in_month function *)

test("Test 1 for number_in_month function", assert_true(number_in_month([(2010, 2, 1), (2000, 2, 28)], 2) = 2));
test("Test 2 for number_in_month function", assert_true(number_in_month([(2010, 2, 1), (2000, 2, 28), (2001, 1, 30)], 2) = 2));
test("Test 3 for number_in_month function", assert_true(number_in_month([(2010, 2, 1)], 2) = 1));
test("Test 4 for number_in_month function", assert_true(number_in_month([(2010, 2, 1), (2000, 2, 28)], 3) = 0));
test("Test 5 for number_in_month function", assert_true(number_in_month([], 2) = 0));

(* Test cases for number_in_months function *)

test("Test 1 for number_in_months function", assert_true(number_in_months([(2010, 2, 1), (2000, 2, 28)], [2]) = 2));
test("Test 2 for number_in_months function", assert_true(number_in_months([(2010, 2, 1), (2000, 2, 28)], [2, 3]) = 2));
test("Test 3 for number_in_months function", assert_true(number_in_months([(2010, 2, 1), (2000, 2, 28), (2001, 3, 31)], [2, 3]) = 3));
test("Test 4 for number_in_months function", assert_true(number_in_months([], [2]) = 0));
test("Test 5 for number_in_months function", assert_true(number_in_months([(2010, 2, 1)], []) = 0));

(* Test cases for dates_in_month function *)

test("Test 1 for dates_in_month function", assert_true(dates_in_month([(2010, 2, 1), (2000, 2, 28)], 2) = [(2010, 2, 1), (2000, 2, 28)]));
test("Test 2 for dates_in_month function", assert_true(dates_in_month([(2010, 2, 1), (2000, 2, 28)], 1) = []));
test("Test 3 for dates_in_month function", assert_true(dates_in_month([(2010, 2, 1), (2000, 2, 28), (2011, 3, 31)], 3) = [(2011, 3, 31)]));
test("Test 4 for dates_in_month function", assert_true(dates_in_month([], 2) = []));

(* Test cases for dates_in_months function *)

test("Test 1 for dates_in_months function", assert_true(dates_in_months([(2010, 2, 1), (2000, 2, 28)], [1, 2]) = [(2010, 2, 1), (2000, 2, 28)]));
test("Test 2 for dates_in_months function", assert_true(dates_in_months([(2010, 2, 1), (2000, 2, 28)], [1]) = []));
test("Test 3 for dates_in_months function", assert_true(dates_in_months([(2010, 2, 1), (2000, 2, 28), (2011, 3, 31)], [2, 3]) = [(2010, 2, 1), (2000, 2, 28), (2011, 3, 31)]));
test("Test 4 for dates_in_months function", assert_true(dates_in_months([], [2]) = []));
test("Test 5 for dates_in_months function", assert_true(dates_in_months([(2010, 2, 1), (2000, 2, 28)], []) = []));

(* Test cases for get_nth function *)

test("Test 1 for get_nth function", assert_true(get_nth(["adcdc", "dsscd", "dccs", "dccc"], 4) = "dccc"));
test("Test 2 for get_nth function", assert_true(get_nth(["adcdc", "dsscd", "dccs", "dccc"], 1) = "adcdc"));
test("Test 3 for get_nth function", assert_true(get_nth(["adcdc"], 1) = "adcdc"));

(* Test cases for date_to_string function *)

test("Test 1 for date_to_string function", assert_true(date_to_string((2013, 12, 12)) = "December 12, 2013"));
test("Test 2 for date_to_string function", assert_true(date_to_string((2013, 1, 12)) = "January 12, 2013"));

(* Test cases for number_before_reaching_sum function *)

test("Test 1 for number_before_reaching_sum function", assert_true(number_before_reaching_sum(10, [1, 2, 3, 4, 5]) = 3));
test("Test 2 for number_before_reaching_sum function", assert_true(number_before_reaching_sum(14, [5, 4, 1, 2, 3]) = 4));
test("Test 3 for number_before_reaching_sum function", assert_true(number_before_reaching_sum(1, [1, 2]) = 0));

(* Test cases for what_month function *)

test("Test 1 for what_month function", assert_true(what_month(10) = 1));
test("Test 2 for what_month function", assert_true(what_month(365) = 12));
test("Test 3 for what_month function", assert_true(what_month(59) = 2));

(* Test cases for month_range function *)

test("Test 1 for month_range", assert_true(month_range(31, 34) = [1, 2, 2, 2]));
test("Test 2 for month_range", assert_true(month_range(58, 61) = [2, 2, 3, 3]));
test("Test 3 for month_range", assert_true(month_range(363, 365) = [12, 12, 12]));

(* Test cases for oldest function *)

test("Test 1 for oldest", assert_true(oldest([]) = NONE));
test("Test 2 for oldest", assert_true(oldest([(2010, 2, 1), (2000, 2, 28), (2001, 1, 30)]) = SOME(2000, 2, 28)));
test("Test 3 for oldest", assert_true(oldest([(2000, 2, 28), (2000, 2, 13)]) = SOME(2000, 2, 13)));
test("Test 3 for oldest", assert_true(oldest([(2000, 2, 28), (2000, 2, 28)]) = SOME(2000, 2, 28)));

(* Run all the test cases *)

run();