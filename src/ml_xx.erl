-module(ml_xx).

-export([f/0]).

f() ->
  ml_ee:f() + ml_cc:f().
