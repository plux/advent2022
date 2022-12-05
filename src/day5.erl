-module(day5).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Stacks = case length(?lines(Input)) > 5 of
                 true ->
                     {[$P, $Z, $M, $T, $R, $C, $N],
                      [$Z, $B, $S, $T, $N, $D],
                      [$G, $T, $C, $F, $R, $Q, $H, $M],
                      [$Z, $R, $G],
                      [$H, $R, $N, $Z],
                      [$D, $L, $Z, $P, $W, $S, $H, $F],
                      [$M, $G, $C, $R, $Z, $D, $W],
                      [$Q, $Z, $W, $H, $L, $F, $J, $S],
                      [$N, $W, $P, $Q, $S]};
                 false ->
                     {[$N, $Z],
                      [$D, $C, $M],
                      [$P]}
             end,
    {part1(?ints_lines(Input), Stacks), part2(?ints_lines(Input), Stacks)}.

part1(Moves, Stacks) ->
    top_stacks(eval_moves(Moves, Stacks, fun move/3)).

part2(Moves, Stacks) ->
    top_stacks(eval_moves(Moves, Stacks, fun move2/3)).

eval_moves(Moves, Stacks, MoveFun) ->
    lists:foldl(
      fun([N, From, To], Acc) ->
              {FromL2, ToL2} = MoveFun(N, element(From, Acc), element(To, Acc)),
              setelement(To, setelement(From, Acc, FromL2), ToL2)
      end, Stacks, Moves).

move(0, From, To) ->
    {From, To};
move(N, [H|From], To) ->
    move(N-1, From, [H|To]).

move2(N, From, To) ->
    {Boxes, From2} = lists:split(N, From),
    {From2, Boxes ++ To}.

top_stacks(Stacks) ->
    [hd(S) || S <- tuple_to_list(Stacks)].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({"CMZ", "MCD"}, ?solve_ex(1))
    , ?_assertEqual({"RTGWZTHLD","STHGRZZFR"}, ?solve())
    ].
