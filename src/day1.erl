-module(day1).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(Input), part2(Input)}.

part1(Input) ->
    lists:max(sum_calories(Input)).

part2(Input) ->
    [A, B, C | _] = lists:reverse(lists:sort(sum_calories(Input))),
    A + B + C.

sum_calories(Input) ->
    [lists:sum(?ints(X)) || X <- ?split(Input, "\n\n")].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({24000, 45000}, ?solve_ex(1))
    , ?_assertEqual({66487, 197301}, ?solve())
    ].
