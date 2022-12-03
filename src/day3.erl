-module(day3).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    lists:sum([score(Line) || Line <- Lines]).

part2([]) -> 0;
part2([A, B, C | Rest]) ->
    prio(intersect(intersect(A, B), C)) + part2(Rest).

score(Line) ->
    {A, B} = lists:split(length(Line) div 2, Line),
    prio(intersect(A, B)).


intersect(A, B) ->
    ordsets:intersection(ordsets:from_list(A), ordsets:from_list(B)).

prio([X]) when X >= $a -> X - $a + 1;
prio([X])              -> X - $A + 27.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({157, 70}, ?solve_ex(1))
    , ?_assertEqual({8105, 2363}, ?solve())
    ].
