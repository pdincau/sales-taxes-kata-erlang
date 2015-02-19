-module(tax_calculator).
-export([price_for/1]).

-include_lib("eunit/include/eunit.hrl").

-define(IMPORT_TAX, 0.05).

price_for({book, Price, not_imported}) ->
    Price;

price_for({book, Price, imported}) ->
    Price + import_tax(Price);

price_for({food, Price, not_imported}) ->
    Price;

price_for({food, Price, imported}) ->
    Price + import_tax(Price).

import_tax(Price) ->
    ?IMPORT_TAX * Price.

not_imported_book_is_not_taxed_test() ->
    Item = {book, 12.49, not_imported},
    ?assertEqual(12.49, tax_calculator:price_for(Item)).

imported_book_has_10_percent_tax_test() ->
    Item = {book, 10.00, imported},
    ?assertEqual(10.50, tax_calculator:price_for(Item)).

not_imported_food_is_not_taxed_test() ->
    Item = {food, 12.49, not_imported},
    ?assertEqual(12.49, tax_calculator:price_for(Item)).

imported_food_has_10_percent_tax_test() ->
    Item = {food, 10.00, imported},
    ?assertEqual(10.50, tax_calculator:price_for(Item)).

