-module(day5).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    [Stacks0, Moves0] = ?split(Input, "\n\n"),
    Stacks = parse_stacks(?lines(Stacks0)),
    Moves = ?ints_lines(Moves0),
    {part1(Moves, Stacks), part2(Moves, Stacks)}.

part1(Moves, Stacks) ->
    top_crates(eval_moves(Moves, Stacks, fun move/3)).

part2(Moves, Stacks) ->
    top_crates(eval_moves(Moves, Stacks, fun move2/3)).

parse_stacks(Lines) ->
    {Diagram, Cols} = lists:split(length(Lines)-1, Lines),
    maps:from_list(
      [ {N, [lists:nth(N*4 - 2, Line) || Line <- Diagram,
                                         length(Line) > (N*4 - 2),
                                         lists:nth(N*4 - 2, Line) >= $A]}
        || N <- ?ints(Cols) ]).

eval_moves(Moves, Stacks, MoveFun) ->
    lists:foldl(
      fun([N, From, To], Acc) ->
              {FromS, ToS} = MoveFun(N, maps:get(From, Acc), maps:get(To, Acc)),
              Acc#{From := FromS, To := ToS}
      end, Stacks, Moves).

move(N, From, To) ->
    {Crates, From2} = lists:split(N, From),
    {From2, lists:reverse(Crates) ++ To}.

move2(N, From, To) ->
    {Crates, From2} = lists:split(N, From),
    {From2, Crates ++ To}.

top_crates(Stacks) ->
    [Crate || {_, [Crate|_]} <- lists:sort(maps:to_list(Stacks))].


-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({"CMZ", "MCD"}, ?solve_ex(1))
    , ?_assertEqual({"RTGWZTHLD","STHGRZZFR"}, ?solve())
    ].
