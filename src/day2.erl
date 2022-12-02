-module(day2).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?words_lines(Input)), part2(?words_lines(Input))}.

part1(Input) ->
    lists:sum([points(parse(B)) + win(parse(A), parse(B)) || [A, B] <- Input]).

part2(Input) ->
    lists:sum([points_outcome(parse(A), parse_outcome(B)) || [A, B] <- Input]).

parse("X") -> rock;
parse("Y") -> paper;
parse("Z") -> scissors;
parse("A") -> rock;
parse("B") -> paper;
parse("C") -> scissors.

parse_outcome("X") -> lose;
parse_outcome("Y") -> draw;
parse_outcome("Z") -> win.

points(rock)     -> 1;
points(paper)    -> 2;
points(scissors) -> 3.

points_outcome(A, B) ->
    Picked = pick(A, B),
    win(A, Picked) + points(Picked).

win(rock,     paper)    -> 6;
win(paper,    scissors) -> 6;
win(scissors, rock)     -> 6;
win(Same,     Same)     -> 3;
win(_,        _)        -> 0.

pick(rock,     lose) -> scissors;
pick(rock,     win)  -> paper;
pick(paper,    lose) -> rock;
pick(paper,    win)  -> scissors;
pick(scissors, lose) -> paper;
pick(scissors, win)  -> rock;
pick(Same,     draw) -> Same.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({15, 12}, ?solve_ex(1))
    , ?_assertEqual({15632, 14416}, ?solve())
    ].
