-module(day1).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?ints(Input)), part2(?ints(Input))}.

part1([]) ->
    ok.

part2(L) ->
    L.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [],
    [ ?_assertEqual(ok, part1(L))
    , ?_assertEqual(ok, part2(L))
%%     , ?_assertEqual({_, _}, ?solve_ex(1))
%%     , ?_assertEqual({_, _}, ?solve())
    ].
