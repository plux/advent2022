-module(day4).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    lists:sum([1 || Line <- Lines, is_fully_overlapping(parse(Line))]).

part2(Lines) ->
    lists:sum([1 || Line <- Lines, is_overlapping(parse(Line))]).

parse(Line) ->
    [?int(X) || Y <- ?split(Line, ","), X <- ?split(Y, "-")].

is_fully_overlapping([FromA, ToA, FromB, ToB]) ->
    (FromA =< FromB andalso ToA >= ToB) orelse
    (FromB =< FromA andalso ToB >= ToA).

is_overlapping([FromA, ToA, FromB, ToB]) ->
    (FromA =< FromB andalso FromB =< ToA) orelse
    (FromB =< FromA andalso FromA =< ToB).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({2, 4}, ?solve_ex(1))
    , ?_assertEqual({540, 872}, ?solve())
    ].
