-module(tax_calculator).
-export([price_for/1]).

-include_lib("eunit/include/eunit.hrl").

-define(STANDARD_TAX, 0.1).
-define(IMPORT_TAX, 0.05).

tax_round(Number, Precision) ->
    P = math:pow(10, Precision),
    erlang:round(Number * P) / P.

price_for({book, Price, not_imported}) ->
    Price;

price_for({book, Price, imported}) ->
    Price + import_tax(Price);

price_for({food, Price, not_imported}) ->
    Price;

price_for({food, Price, imported}) ->
    tax_round(Price + import_tax(Price), 2);

price_for({medical, Price, not_imported}) ->
    Price;

price_for({general, Price, not_imported}) ->
    tax_round(Price + standard_tax(Price), 2).

import_tax(Price) ->
    ?IMPORT_TAX * Price.

standard_tax(Price) ->
    tax_round(?STANDARD_TAX * Price, 2).

not_imported_book_is_not_taxed_test() ->
    Item = {book, 12.49, not_imported},
    ?assertEqual(12.49, tax_calculator:price_for(Item)).

imported_book_has_5_percent_tax_test() ->
    Item = {book, 10.00, imported},
    ?assertEqual(10.50, tax_calculator:price_for(Item)).

not_imported_food_is_not_taxed_test() ->
    Item = {food, 12.49, not_imported},
    ?assertEqual(12.49, tax_calculator:price_for(Item)).

imported_food_has_5_percent_tax_test() ->
    Item = {food, 10.00, imported},
    ?assertEqual(10.50, tax_calculator:price_for(Item)).

not_imported_medical_is_not_taxed_test() ->
    Item = {medical, 9.75, not_imported},
    ?assertEqual(9.75, tax_calculator:price_for(Item)).

not_imported_general_item_has_10_percent_tax_test() ->
    Item = {general, 18.99, not_imported},
    ?assertEqual(20.89, tax_calculator:price_for(Item)).
