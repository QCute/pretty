-module(test).
-export([test/0]).

test() ->
    %% setup
    code:ensure_loaded(pretty),
    %% print
    io:format("~p ~tp~n", ["hello", world]).
