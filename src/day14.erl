-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Grid = parse(Input),
    {part(Grid, 1), part(Grid, 2)}.

parse(Input) ->
    maps:from_list(lists:flatten([to_line(L) || L <- ?ints_lines(Input)])).

to_line([_,_]) ->
    [];
to_line([FromX, FromY | [ToX, ToY | _] = Rest]) ->
    [to_line(FromX, FromY, ToX, ToY) | to_line(Rest)].

to_line(X, FromY, X, ToY) when FromY < ToY ->
    [{{X, Y}, $#} || Y <- lists:seq(FromY, ToY)];
to_line(X, FromY, X, ToY) ->
    [{{X, Y}, $#} || Y <- lists:seq(ToY, FromY)];
to_line(FromX, Y, ToX, Y) when FromX < ToX ->
    [{{X, Y}, $#} || X <- lists:seq(FromX, ToX)];
to_line(FromX, Y, ToX, Y) ->
    [{{X, Y}, $#} || X <- lists:seq(ToX, FromX)].

-define(start, {500, 0}).
part(Grid, Part) ->
    Limit = lists:max([Y || {_, Y} <- maps:keys(Grid)]),
    length([1 || $o <- maps:values(drop_sand(?start, [], Grid, Limit, Part))]).

drop_sand({_X, Y}, _Prev, Grid, Limit, _Part = 1) when Y == Limit ->
    Grid;
drop_sand({X, Y}, Prev, Grid, Limit, Part = 2) when Y == Limit+1 ->
    drop_sand(hd(Prev), tl(Prev), Grid#{{X, Y} => $o}, Limit, Part);
drop_sand({X, Y}, Prev, Grid, Limit, Part) ->
    Points = [{X, Y}, {X-1, Y+1}, {X, Y+1}, {X+1, Y+1}],
    case [maps:is_key(P, Grid) || P <- Points] of
        [true, _, _, _] ->
            Grid;
        [_, true, true, true] when Prev == [] ->
            drop_sand(?start, [], Grid#{{X, Y} => $o}, Limit, Part);
        [_, true, true, true] ->
            drop_sand(hd(Prev), tl(Prev), Grid#{{X, Y} => $o}, Limit, Part);
        [_, true, true, _] ->
            drop_sand({X+1, Y+1}, [{X, Y}|Prev], Grid, Limit, Part);
        [_, _, true, _] ->
            drop_sand({X-1, Y+1}, [{X, Y}|Prev], Grid, Limit, Part);
        [_, _, _, _] ->
            drop_sand({X, Y+1}, [{X, Y}|Prev], Grid, Limit, Part)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({24, 93}, ?solve_ex(1))
    , ?_assertEqual({655, 26484}, ?solve())
    ].
