-module(call_chains).

%% xref:start(s), xref:add_application(s, "_build/default/lib/riak_core", [{builtins, true}]).

%% spawn(fun() -> erlang:display(call_chains:xx("nif_wrapper")) end).

-export([f/1, d/0, ff/1, xx/1]).


xx(Q) ->
  {ok, L} = xref:q(s, "E || " ++ Q),
  expand_cl(L).

expand_cl([]) -> [];
expand_cl([{Caller, Callee} | Calls]) ->
  CallsOfCaller = query_function_call(Caller),
  expand_cl(CallsOfCaller, [Caller, Callee], #{Caller => true})
    ++ expand_cl(Calls).

expand_cl([], CallChain, _ExploredCallers) ->
  [CallChain];
expand_cl([{Caller, _Callee} | Calls], CallChain, ExploredCallers) ->
  case ExploredCallers of
    #{Caller := true} -> [];
    _ ->
      CallsOfCaller = query_function_call(Caller),
      case Calls of
        [] ->
          expand_cl(CallsOfCaller, [Caller | CallChain], ExploredCallers#{Caller => true});
        _ ->
          expand_cl(CallsOfCaller, [Caller | CallChain], ExploredCallers#{Caller => true})
            ++ expand_cl(Calls, CallChain, ExploredCallers#{Caller => true})
      end
  end.

query_function_call(Call) ->
  {ok, L} = xref:q(s, "E ||" ++ mfa_to_list(Call)),
  L.


ff(Q) ->
  {ok, L} = xref:q(s, "E || " ++ Q),

  CallList = lists:map(fun({Caller, Callee}) ->
                           CallerCallers = query_function_call(Caller),
                           expand_call_list(CallerCallers, [Caller, Callee], #{Caller => true})
                       end, L),
  CallList.

expand_call_list([], CallList, _) ->
  erlang:display(CallList),
  CallList;
expand_call_list(L, CallList, ExploredCallers) ->
  lists:map(fun({Caller, _Callee}) ->
                case maps:is_key(Caller, ExploredCallers) of
                  true -> [];
                  false ->
                    CallerCallers = query_function_call(Caller),
                    expand_call_list(CallerCallers,
                                     [Caller | CallList],
                                     ExploredCallers#{Caller => true})
                end
            end, L).

d() ->
  {ok, L} = xref:q(s, "closure E || nif_wrapper"),
  lists:map(fun({Caller, Callee}) ->
                {CallerM, _, _} = Caller,
                {CalleeM, _, _} = Callee,
                Query =
                  lists:flatten(io_lib:format("{~p, ~p}:Mod of ME", [CallerM, CalleeM])),
                {ok, ModuleChains} = xref:q(s, Query),
                {ModuleChains, Caller, Callee}
            end,  L).

f(Q) ->
  {ok, L} = xref:q(s, "E || " ++ Q),
  expand_caller(L, #{}).

expand_caller(L, CallsMap) ->
  NewCallsMap =
    lists:foldl(fun({Caller, Callee}, Acc) ->
                    CallerCallers = query_function_call(Caller),
                    append_callers(CallerCallers, [Caller, Callee], Acc)
                end,
                CallsMap, L),
  expand_call_map(NewCallsMap).

expand_call_map(Map) ->
  Continues = maps:filter(fun(_K, V) -> V =:= continue end, Map),
  erlang:display(maps:size(Continues)),
  case maps:size(Continues) > 0 of
    true ->
      NewMap =
        maps:fold(fun(_, stop, AccIn) ->
                      AccIn;
                     ([Caller | Others], continue, AccIn) ->
                      CallerCallers = query_function_call(Caller),
                      append_callers(CallerCallers, [Caller | Others], AccIn)
                  end, #{}, Map),
      expand_call_map(NewMap);
    _ ->
      Map
  end.



mfa_to_list({M,F,A}) ->
  atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A).


append_callers([], CallList, Acc) ->
  Acc#{CallList => stop};
append_callers(Callers, CallList, Acc) ->
  lists:foldl(fun({Caller, _Callee}, FoldAcc) ->
                FoldAcc#{[Caller | CallList] => continue}
              end, Acc, Callers).
