-module(ml_cc).


-export([f/0]).

f() ->
  ml_dd:f() + ml_xx:f().
