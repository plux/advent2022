-module(day10).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?words_lines(Input)), part2(?words_lines(Input))}.

part1(Input) ->
    Trace = eval(Input, [1]),
    Checkpoints = [20, 60, 100, 140, 180, 220],
    lists:sum([lists:nth(P-1, Trace) * P || P <- Checkpoints]).

part2(Input) ->
    Trace = eval(Input, [1]),
    Pixels = blit(Trace, 0),
    aoc:ocr(?grid(lists:sublist([ lists:sublist([$#|Chunk], 40)
                                  || Chunk <- ?chunks(Pixels, 40)], 6))).

eval([], Trace) ->
    lists:reverse(Trace);
eval([["noop"]|Rest], [X|_] = Trace) ->
    eval(Rest, [X|Trace]);
eval([["addx", IntStr]|Rest], [X|_]=Trace) ->
    eval([["noop"]|Rest], [X + ?int(IntStr)|Trace]).

blit([], _) ->
    [];
blit(Trace, 40) ->
    blit(Trace, 0);
blit([X|Rest], N) when X == N; X == N+1; X == N+2 ->
    [$#|blit(Rest, N+1)];
blit([_|Rest], N) ->
    [$.|blit(Rest, N+1)].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertMatch({13140, _}, ?solve_ex(2))
    , ?_assertEqual({14760, "EFGERURE"}, ?solve())
    ].
