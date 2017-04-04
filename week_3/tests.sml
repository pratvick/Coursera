(* Test cases for all_except_option function *)
test("Test 1 for all_except_option function", assert_true(all_except_option ("string", ["string"]) = SOME []));
test("Test 2 for all_except_option function", assert_true(all_except_option ("string", ["string", "s", "s1"]) = SOME ["s", "s1"]));
test("Test 3 for all_except_option function", assert_true(all_except_option ("string", ["s"]) = NONE));
test("Test 4 for all_except_option function", assert_true(all_except_option ("string", []) = NONE));

(* Test cases for get_substitutions1 function *)
test("Test 1 for get_substitutions1 function", assert_true(get_substitutions1 ([["foo"], ["there"]], "foo") = []));
test("Test 2 for get_substitutions1 function", assert_true(get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"]));
test("Test 3 for get_substitutions1 function", assert_true(get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"]));

(* Test cases for get_substitutions2 function *)
test("Test 1 for get_substitutions2 function", assert_true(get_substitutions2 ([["foo"], ["there"]], "foo") = []));
test("Test 2 for get_substitutions2 function", assert_true(get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"]));
test("Test 3 for get_substitutions2 function", assert_true(get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"]));
test("Test 4 for get_substitutions2 function", assert_true(get_substitutions2 ([], "Jeff") = []));

(* Test cases for similar_names function *)
test("Test 1 for similar_names function", assert_true(similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]));
test("Test 2 for similar_names function", assert_true(similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]));
test("Test 3 for similar_names function", assert_true(similar_names ([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", middle="W", last="Smith"}]));

(* Test cases for card_color function *)
test("Test 1 for card_color function", assert_true(card_color (Clubs, Num 2) = Black));
test("Test 2 for card_color function", assert_true(card_color (Spades, Ace) = Black));
test("Test 3 for card_color function", assert_true(card_color (Hearts, Queen) = Red));
test("Test 4 for card_color function", assert_true(card_color (Diamonds, Jack) = Red));

(* Test cases for card_value function *)
test("Test 1 for card_value function", assert_true(card_value (Clubs, Num 2) = 2));
test("Test 2 for card_value function", assert_true(card_value (Spades, Ace) = 11));
test("Test 3 for card_value function", assert_true(card_value (Diamonds, Jack) = 10));
test("Test 4 for card_value function", assert_true(card_value (Hearts, Queen) = 10));

(* Test cases for remove_card function *)
test("Test 1 for remove_card function", assert_true(remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []));
test("Test 2 for remove_card function", assert_true(remove_card ([(Hearts, Ace), (Spades, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Ace)]));
test("Test 3 for remove_card function", assert_true(remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]));

(* Test cases for all_same_color function *)
test("Test 1 for all_same_color function", assert_true(all_same_color [(Hearts, Ace), (Hearts, Ace)] = true));
test("Test 2 for all_same_color function", assert_true(all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true));
test("Test 3 for all_same_color function", assert_true(all_same_color [(Hearts, Ace), (Spades, Ace)] = false));
test("Test 4 for all_same_color function", assert_true(all_same_color [] = true));

(* Test cases for sum_cards function *)
test("Test 1 for sum_cards function", assert_true(sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4));
test("Test 2 for sum_cards function", assert_true(sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13));
test("Test 3 for sum_cards function", assert_true(sum_cards [(Clubs, Num 2),(Clubs, King)] = 12));
test("Test 4 for sum_cards function", assert_true(sum_cards [] = 0));

(* Test cases for score function *)
test("Test 1 for score function", assert_true(score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4));
test("Test 2 for score function", assert_true(score ([(Hearts, Num 10),(Spades, Num 4)],10) = 12));
test("Test 3 for score function", assert_true(score ([(Hearts, Num 10)],10) = 0));
test("Test 4 for score function", assert_true(score ([(Hearts, Num 10),(Diamonds, Num 4)],10) = 6));

(* Test cases for officiate function *)
test("Test 1 for officiate function", assert_true(officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6));
test("Test 2 for officiate function", assert_true(officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw, Discard (Hearts, Num 2)], 15) = 7));
test("Test 3 for officiate function", assert_true(officiate([(Hearts, Num 2),(Clubs, Num 4)],[], 15) = 7));
test("Test 4 for officiate function", assert_true(officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw, Draw, Draw], 15) = 9));

run()