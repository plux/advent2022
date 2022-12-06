-module(day6).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(Input), part2(Input)}.

part1(Input) ->
    find_marker(Input, 4).

part2(Input) ->
    find_marker(Input, 14).

find_marker(Input, MarkerLen) ->
    Chars = lists:sublist(Input, MarkerLen),
    case lists:uniq(Chars) of
        Chars -> MarkerLen;
        _     -> 1 + find_marker(tl(Input), MarkerLen)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({7, 19}, ?solve_ex(1))
    , ?_assertEqual({1794, 2851}, ?solve())
    ].
