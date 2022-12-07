-module(day7).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {[], Usage} = eval(?words_lines(Input), [], #{}),
    {part1(Usage), part2(Usage)}.

part1(Usage) ->
    lists:sum([Size || Size <- maps:values(Usage), Size =< 100_000]).

part2(Usage) ->
    {_, Used} = counter:max(Usage),
    lists:min([Size || Size <- maps:values(Usage), Used - Size < 40_000_000]).

eval([], _Path, Usage) ->
    {[], Usage};
eval([["dir", _Dir]|Rest], Path, Usage) ->
    eval(Rest, Path, Usage);
eval([["$", "ls"]|Rest], Path, Usage) ->
    eval(Rest, Path, Usage);
eval([["$", "cd", ".."]|Rest], _Path, Usage) ->
    {Rest, Usage};
eval([["$", "cd", Dir]|Rest0], Path, Usage0) ->
    {Rest, #{[Dir|Path] := Sum} = Usage} = eval(Rest0, [Dir|Path], Usage0),
    eval(Rest, Path, counter:incr(Usage, Path, Sum));
eval([[Size, _File]|Rest], Path, Usage) ->
    eval(Rest, Path, counter:incr(Usage, Path, ?int(Size))).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({95437, 24933642}, ?solve_ex(1))
    , ?_assertEqual({1118405, 12545514}, ?solve())
    ].
