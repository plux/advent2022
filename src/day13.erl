-module(day13).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1(Packets) ->
    Compared = [cmp(L, R) || [L, R] <- ?chunks(Packets, 2)],
    lists:sum([I || {I, lt} <- lists:enumerate(Compared)]).

part2(Packets) ->
    Dividers = [[[2]], [[6]]],
    Sorted = lists:sort(fun(L, R) -> lt == cmp(L, R) end, Packets ++ Dividers),
    aoc:product(
      [I || {I, P} <- lists:enumerate(Sorted), lists:member(P, Dividers)]).

parse(Input) ->
    [string_to_term(L) || L <- ?lines(Input)].

string_to_term(Str) ->
    {ok, Tokens, _EndLine} = erl_scan:string(Str ++ "."),
    {ok, AbsForm}          = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs}    = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

cmp(X, X) ->
    eq;
cmp(L, R) when is_integer(L), is_integer(R), L < R ->
    lt;
cmp(L, R) when is_integer(L), is_integer(R), L > R ->
    gt;
cmp(L, R) when is_integer(L) ->
    cmp([L], R);
cmp(L, R) when is_integer(R) ->
    cmp(L, [R]);
cmp([L|LT], [R|RT]) ->
    case cmp(L, R) of
        eq  -> cmp(LT, RT);
        Cmp -> Cmp
    end;
cmp([], [_|_]) ->
    lt;
cmp([_|_], []) ->
    gt.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({13, 140}, ?solve_ex(1))
    , ?_assertEqual({6484, 19305}, ?solve())
    ].
