-module(day9).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?words_lines(Input)), part2(?words_lines(Input))}.

part1(Lines) ->
    move(Lines, 2).

part2(Lines) ->
    move(Lines, 10).

move(Lines, Length) ->
    Rope = lists:duplicate(Length, {0, 0}),
    Moves = [{direction(Dir), ?int(Steps)} || [Dir, Steps] <- Lines],
    move(Moves, Rope, #{}).

direction("R") -> ?east;
direction("L") -> ?west;
direction("D") -> ?south;
direction("U") -> ?north.

move([], _, Visited) ->
    maps:size(Visited);
move([{_, 0}|Rest], Rope, Visited) ->
    move(Rest, Rope, Visited);
move([{{DX, DY} = Dir, N}|Rest], [{HX, HY}|Tails], Visited) ->
    Rope = move_rope([{HX+DX, HY+DY}|Tails]),
    move([{Dir, N-1}|Rest], Rope, Visited#{lists:last(Rope) => $#}).

move_rope([T]) ->
    [T];
move_rope([H, T0|Rest]) ->
    [H|move_rope([move_tail(H, T0)|Rest])].

move_tail({HX, HY}, {TX, TY}) when abs(HX - TX) < 2,
                                   abs(HY - TY) < 2  -> {TX, TY};
move_tail({HX, HY}, {TX, TY}) when HX == TX, HY > TY -> {TX, TY+1};
move_tail({HX, HY}, {TX, TY}) when HX == TX, HY < TY -> {TX, TY-1};
move_tail({HX, HY}, {TX, TY}) when HY == TY, HX > TX -> {TX+1, TY};
move_tail({HX, HY}, {TX, TY}) when HY == TY, HX < TX -> {TX-1, TY};
move_tail({HX, HY}, {TX, TY}) when HY  < TY, HX < TX -> {TX-1, TY-1};
move_tail({HX, HY}, {TX, TY}) when HY  > TY, HX < TX -> {TX-1, TY+1};
move_tail({HX, HY}, {TX, TY}) when HY  < TY, HX > TX -> {TX+1, TY-1};
move_tail({HX, HY}, {TX, TY}) when HY  > TY, HX > TX -> {TX+1, TY+1}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertMatch({13, _}, ?solve_ex(1))
    , ?_assertMatch({_, 36}, ?solve_ex(2))
    , ?_assertEqual({6522, 2717}, ?solve())
    ].
