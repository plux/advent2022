-module(day11).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   Monkeys = [parse(Chunk) || Chunk <- ?chunks(?words_lines(Input), 6)],
   {part1(Monkeys), part2(Monkeys)}.

parse([["Monkey",[Nr,$:]],
       ["Starting","items:"|Items],
       ["Operation:","new","=","old",Op0,Operand],
       ["Test:","divisible","by",Divisor],
       ["If","true:","throw","to","monkey",TrueMonkey],
       ["If","false:","throw","to","monkey",FalseMonkey]]) ->
    Op = list_to_atom(Op0),
    #{nr => Nr - $0,
      items => [?int(I) || I <- Items],
      op => case aoc:is_int(Operand) of
                true  -> fun(X) -> erlang:Op(X, ?int(Operand)) end;
                false -> fun(X) -> erlang:Op(X, X) end
            end,
      divisor => ?int(Divisor),
      true_monkey => ?int(TrueMonkey),
      false_monkey => ?int(FalseMonkey),
      inspected => 0
     }.

part1(Monkeys) ->
    Items = maps:from_list([{Nr, Is} || #{nr := Nr, items := Is} <- Monkeys]),
    eval(Monkeys, Items, fun(X) -> X div 3 end, 20).

eval(Monkeys, _Items, _ReduceWorry, 0) ->
    Inspected = lists:reverse(lists:sort([I || #{inspected := I} <- Monkeys])),
    [A, B|_] = Inspected,
    A*B;
eval(Monkeys0, Items0, ReduceWorry, Count) ->
    {Monkeys, Items} =
        lists:mapfoldl(
          fun(Monkey, Acc) ->
                  #{nr := Nr,
                    op := Op,
                    divisor := Divisor,
                    true_monkey := TrueMonkey,
                    false_monkey := FalseMonkey,
                    inspected := Inspected} = Monkey,
                  Levels = [ReduceWorry(Op(L)) || L <- maps:get(Nr, Acc)],
                  {TrueLevels, FalseLevels} =
                      lists:partition(fun(L) -> L rem Divisor == 0 end, Levels),
                  {Monkey#{inspected => Inspected + length(Levels)},
                   Acc#{Nr := [],
                        TrueMonkey := maps:get(TrueMonkey, Acc) ++ TrueLevels,
                        FalseMonkey := maps:get(FalseMonkey, Acc) ++ FalseLevels
                       }}
                    end, Items0, Monkeys0),
    eval(Monkeys, Items, ReduceWorry, Count-1).


part2(Monkeys) ->
    Items = maps:from_list([{Nr, Is} || #{nr := Nr, items := Is} <- Monkeys]),
    DivisorProduct = lists:foldl(fun(#{divisor := D0}, Acc) ->
                                         D0 * Acc
                                 end, 1, Monkeys),
    eval(Monkeys, Items, fun(X) -> X rem DivisorProduct end, 10_000).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({10605, 2713310158}, ?solve_ex(1))
    , ?_assertEqual({66802, 21800916620}, ?solve())
    ].
