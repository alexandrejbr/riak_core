-module(nif_wrapper).

-export([list_to_atom/1, binary_to_atom/2, f/0]).

list_to_atom(S) -> erlang:list_to_atom(S).

binary_to_atom(S, M) -> erlang:binary_to_atom(S, M).

f() ->
  {ok, L} = xref:q(s, "closure E || nif_wrapper"),
  expand_caller(L, #{}).

expand_caller(L, CallsMap) ->
  lists:foldl(fun({Caller, Callee}, Acc) ->
                  CallerCallers = query_function_call(Caller),
                  append_callers(CallerCallers, Caller, Callee, Acc)
              end,
              CallsMap, L).

mfa_to_list({M,F,A}) ->
  atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A).

query_function_call(Call) ->
  {ok, L} = xref:q(s, "closure E ||" ++ mfa_to_list(Call)),
  L.

append_callers([], Caller, EndCallee, Acc) ->
  Acc#{Caller => EndCallee};
append_callers(Callers, _Callee, EndCallee, Acc) ->
  lists:foldl(fun({Caller, Callee}, FoldAcc) ->
                FoldAcc#{[Caller, Callee] => EndCallee}
              end, Acc, Callers).
