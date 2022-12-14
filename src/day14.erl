-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Grid = parse(Input),
    {part(Grid, 1), part(Grid, 2)}.

parse(Input) ->
    to_grid(?ints_lines(Input), #{}).

to_grid([], Grid) ->
    Grid;
to_grid([Line|Rest], Grid) ->
     to_grid(Rest,
             maps:merge(Grid, maps:from_list(lists:flatten(to_line(Line))))).

to_line([_,_]) ->
    [];
to_line([FromX, FromY, ToX, ToY|Rest]) ->
    [to_line(FromX, FromY, ToX, ToY)|to_line([ToX, ToY|Rest])].

to_line(X, FromY, X, ToY) when FromY < ToY ->
    [{{X, Y}, $#} || Y <- lists:seq(FromY, ToY)];
to_line(X, FromY, X, ToY) ->
    [{{X, Y}, $#} || Y <- lists:seq(ToY, FromY)];
to_line(FromX, Y, ToX, Y) when FromX < ToX ->
    [{{X, Y}, $#} || X <- lists:seq(FromX, ToX)];
to_line(FromX, Y, ToX, Y) ->
    [{{X, Y}, $#} || X <- lists:seq(ToX, FromX)].

part(Grid, Part) ->
    Start = {500, 0},
    Limit = lists:max([Y || {_, Y} <- maps:keys(Grid)]),
    Grid2 = drop_sand(Start, Grid, Limit, Part),
    length([1 || $o <- maps:values(Grid2)]).

drop_sand({_X, Y}, Grid, Limit, Part) when Y == Limit, Part == 1 ->
    Grid;
drop_sand({X, Y}, Grid, Limit, Part) when Y+1 == Limit+2, Part == 2 ->
    drop_sand({500, 0}, Grid#{{X, Y} => $o}, Limit, Part);
drop_sand({X, Y}, Grid, Limit, Part) ->
    Points = [{X, Y}, {X-1, Y+1}, {X, Y+1}, {X+1, Y+1}],
    case [maps:is_key(P, Grid) || P <- Points] of
        [true, _, _, _] ->
            Grid;
        [_, true, true, true] ->
            drop_sand({500, 0}, Grid#{{X, Y} => $o}, Limit, Part);
        [_, true, true, _] ->
            drop_sand({X+1, Y+1}, Grid, Limit, Part);
        [_, _, true, _] ->
            drop_sand({X-1, Y+1}, Grid, Limit, Part);
        [_, _, _, _] ->
            drop_sand({X, Y+1}, Grid, Limit, Part)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({24, 93}, ?solve_ex(1))
    , ?_assertEqual({655, 26484}, ?solve())
    ].
