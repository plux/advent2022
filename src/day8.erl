-module(day8).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Grid = ?grid(?lines(Input)),
    {part1(Grid), part2(Grid)}.

part1(Grid) ->
    Rows = aoc:grid_rows(Grid),
    Cols = aoc:transpose(Rows),
    Coords = lists:flatten(
               [ [check(Row, 0) ++ check(lists:reverse(Row), 0) || Row <- Rows]
               , [check(Col, 0) ++ check(lists:reverse(Col), 0) || Col <- Cols]
               ]),
    length(lists:usort(Coords)).

part2(Grid) ->
    lists:max([scenic_score(K, V, Grid) || {K, V} <- maps:to_list(Grid)]).

check([], _Prev) ->
    [];
check([{Coord, Size}|Rest], Prev) when Size > Prev ->
    [Coord|check(Rest, Size)];
check([_|Rest], Prev) ->
    check(Rest, Prev).

scenic_score(Coord, Size, Grid) ->
    view_distance(Coord, Size, Grid, ?north) *
    view_distance(Coord, Size, Grid, ?west) *
    view_distance(Coord, Size, Grid, ?east) *
    view_distance(Coord, Size, Grid, ?south).

view_distance({X, Y}, Size, Grid, {DX, DY}) ->
    NextTree = {X+DX, Y+DY},
    case maps:find(NextTree, Grid) of
        {ok, NextSize} when Size > NextSize ->
            1 + view_distance(NextTree, Size, Grid, {DX, DY});
        {ok, _NextSize} ->
            1;
        error ->
            0
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({21, 8}, ?solve_ex(1))
    , ?_assertEqual({1662, 537600}, ?solve())
    ].
