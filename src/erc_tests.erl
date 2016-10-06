-module(erc_tests).
-include_lib("eunit/include/eunit.hrl").  % EUnit unit testing framework


% check that the API functions are exported properly.
export_test_() ->
    [?_assert(erlang:function_exported(erc, start, 0)),
     ?_assert(erlang:function_exported(erc, connect, 2)),
     ?_assert(erlang:function_exported(erc, chat, 2)),
     ?_assert(erlang:function_exported(erc, history, 1)),
     ?_assert(erlang:function_exported(erc, filter, 3)),
     ?_assert(erlang:function_exported(erc, plunk, 2)),
     ?_assert(erlang:function_exported(erc, censor, 2))
    ].


% check the expected behaviour of erc:start()
start_test_() ->
    [?_assertMatch({ok, Pid} when is_pid(Pid), erc:start())].


% check the expected behaviour of erc:connect(Server, Nick)
connect_test_() ->
    {_, S} = erc:start(),
    [?_assertMatch({ok, Ref} when is_reference(Ref), erc:connect(S, andrew)),
     ?_assertMatch({ok, Ref} when is_reference(Ref), erc:connect(S, 'Nikolaj')),
     ?_assertThrow('connect: bad inputs', erc:connect(andrew, S)),
     ?_assertThrow('connect: bad Nick', erc:connect(S, 11)),
     ?_assertThrow('connect: bad Server', erc:connect(11, andrew))
    ].