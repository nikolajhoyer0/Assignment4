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
    {_, TestRef} = erc:connect(S, spiderman),
    [?_assert(erlang:is_reference(TestRef)),
     ?_assert(erlang:is_pid(S)),
     ?_assertMatch({ok, Ref} when is_reference(Ref), erc:connect(S, andrew)),
     ?_assertMatch({ok, Ref} when is_reference(Ref), erc:connect(S, 'Nikolaj')),
     ?_assertThrow('connect: bad inputs', erc:connect(andrew, S)),
     ?_assertThrow('connect: bad Nick', erc:connect(S, 11)),
     ?_assertThrow('connect: bad Server', erc:connect(11, andrew)),
     ?_assertEqual({ok, TestRef}, erc:connect(S, batman)),
     ?_assertEqual({ok, TestRef}, erc:connect(S, superman)),
     ?_assertEqual({ok, TestRef}, erc:connect(S, ironman))
    ].


% check the expected behaviour of erc:chat(Server, Cont).
chat_test_() ->
    {_, S} = erc:start(),
    erc:connect(S, spiderman),
    erc:chat(S, " "),
    Hist1 = erc:history(S),
    erc:chat(S, "With great power..."),
    Hist2 = erc:history(S),
    erc:chat(S, "comes great responsibility"),
    Hist3 = erc:history(S),
    [?_assert(Hist1 == [ {spiderman, " "}
                       ]),
     ?_assert(Hist2 == [ {spiderman, "With great power..."},
                         {spiderman, " "}
                       ]),
     ?_assert(Hist3 == [ {spiderman, "comes great responsibility"},
                         {spiderman, "With great power..."},
                         {spiderman, " "}
                       ]),
     ?_assertThrow('chat: bad message', erc:chat(S, ""))
    ].


