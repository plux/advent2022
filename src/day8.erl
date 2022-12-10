-module(day8).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Grid = ?grid(?lines([X || X <- Input])),
    {part1(Grid), part2(Grid)}.

part1(Grid) ->
    Rows = aoc:grid_rows(Grid),
    Cols = aoc:transpose(Rows),
    Coords = lists:flatten(
               [ [check(Row, 0) ++ check(lists:reverse(Row), 0) || Row <- Rows]
               , [check(Col, 0) ++ check(lists:reverse(Col), 0) || Col <- Cols]
               ]),
    length(lists:usort(Coords)).

check([], _Prev) ->
    [];
check([{X, Y, Size}|Rest], Prev) when Size > Prev ->
    [{X, Y}|check(Rest, Size)];
check([_|Rest], Prev) ->
    check(Rest, Prev).

part2(_Grid) ->
    ok.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({21, ok}, ?solve_ex(1))
    , ?_assertEqual({1662, ok}, ?solve())
    ].
