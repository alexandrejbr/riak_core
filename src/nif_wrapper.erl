-module(nif_wrapper).

-export([list_to_atom/1, binary_to_atom/2]).

list_to_atom(S) -> erlang:list_to_atom(S).

binary_to_atom(S, M) -> erlang:binary_to_atom(S, M).
