test("is_older: true if first year is older",
  assert_true(is_older((2000, 0, 0), (2010, 0, 0))));

test("is_older: false if first year is newer",
  assert_false(is_older((2010, 0, 0), (2000, 0, 0))));

test("number_in_month: true if list of months have months with the same month number",
  assert_true(number_in_month([(2010, 2, 1), (2000, 2, 28)], 2) = 2));

run();
