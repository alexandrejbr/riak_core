-module(nif_wrapper).

-export([list_to_atom/1, binary_to_atom/2, f/0]).

list_to_atom(S) -> erlang:list_to_atom(S).

binary_to_atom(S, M) -> erlang:binary_to_atom(S, M).

f() ->
  {ok, L} = xref:q(s, "closure E || nif_wrapper"),
  expand_caller(L, #{}).

expand_caller(L, CallsMap) ->
  NewCallsMap = 
    lists:foldl(fun({Caller, Callee}, Acc) ->
                    CallerCallers = query_function_call(Caller),
                    append_callers(CallerCallers, [Caller, Callee], Acc)
                end,
                CallsMap, L),
  %% maps:fold(fun(_, stop, AccIn) -> 
  %%               AccIn;
  %%              ([Caller | Others], continue, AccIn) ->
  %%               CallerCallers = query_function_call(Caller),
  %%               ok
  %%           end, #{}, NewCallsMap).
  NewCallsMap.

mfa_to_list({M,F,A}) ->
  atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A).

query_function_call(Call) ->
  {ok, L} = xref:q(s, "closure E ||" ++ mfa_to_list(Call)),
  L.

append_callers([], CallList, Acc) ->
  Acc#{CallList => stop};
append_callers(Callers, CallList, Acc) ->
  lists:foldl(fun({Caller, _Callee}, FoldAcc) ->
                FoldAcc#{[Caller | CallList] => continue}
              end, Acc, Callers).
