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
     ?_assertEqual({ok, TestRef}, erc:connect(S, ironman)),
     ?_assertEqual({error, spiderman, is_taken}, erc:connect(S, spiderman))
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


chat2_test_() ->
    MainId = self(),
    {_, Server} = erc:start(),
    _User1 = spawn(fun() -> user(Server, MainId, spiderman, 13) end),
    _User2 = spawn(fun() -> user(Server, MainId, batman, 11) end),
    % io:fwrite("Hello from chat2_test!~n", []),
    receive
        {_User1, done} -> ok
    end,
    % io:fwrite("chat2_test: user1 done!~n", []),
    receive
        {_User2, done} -> ok
    end,
    % io:fwrite("chat2_test: user1 done!~n", []),
    % check state of the server, we cannot know what order the messages will
    % arrive in, but we can check that they all arrived!
    Hist = lists:sort(erc:history(Server)),
    io:fwrite("~62p~n", [Hist]),
    [?_assert(Hist == [ {batman,"batman sent msg no. 0"},
                        {batman,"batman sent msg no. 1"},
                        {batman,"batman sent msg no. 10"},
                        {batman,"batman sent msg no. 11"},
                        {batman,"batman sent msg no. 2"},
                        {batman,"batman sent msg no. 3"},
                        {batman,"batman sent msg no. 4"},
                        {batman,"batman sent msg no. 5"},
                        {batman,"batman sent msg no. 6"},
                        {batman,"batman sent msg no. 7"},
                        {batman,"batman sent msg no. 8"},
                        {batman,"batman sent msg no. 9"},
                        {spiderman,"spiderman sent msg no. 0"},
                        {spiderman,"spiderman sent msg no. 1"},
                        {spiderman,"spiderman sent msg no. 10"},
                        {spiderman,"spiderman sent msg no. 11"},
                        {spiderman,"spiderman sent msg no. 12"},
                        {spiderman,"spiderman sent msg no. 13"},
                        {spiderman,"spiderman sent msg no. 2"},
                        {spiderman,"spiderman sent msg no. 3"},
                        {spiderman,"spiderman sent msg no. 4"},
                        {spiderman,"spiderman sent msg no. 5"},
                        {spiderman,"spiderman sent msg no. 6"},
                        {spiderman,"spiderman sent msg no. 7"},
                        {spiderman,"spiderman sent msg no. 8"},
                        {spiderman,"spiderman sent msg no. 9"}
                      ])
    ].


user(Server, MainId, Nick, Count) ->
    Me = self(),
    io:fwrite("Hello from user: ~p~n", [Nick]),
    % send a connect request to the server
    Server ! {Me, {connect, Nick}},
    io:fwrite("Sent connect request: ~p~n", [Nick]),
    receive
        {Server, {ok, Ref}} when is_reference(Ref) ->
            io:fwrite("Connect request approved: ~p~n", [Nick]),
            chat_it_up(Server, MainId, Nick, Count);
        {Server, {error, Nick, is_taken}}  -> ok;
        Reply ->
            io:fwrite("Connect request denied: ~p~n", [Reply])
    end.


chat_it_up(Server, MainId, Nick, Count) when is_integer(Count)
                                        andalso (Count > -1) ->
    io:fwrite("Hello from chat_it_up: ~p~n", [Nick]),
    % construct a unique message
    Msg = erlang:atom_to_list(Nick)
          ++ " sent msg no. "
          ++ integer_to_list(Count),
    % send a chat request ot the server
    io:fwrite("Hello from chat_it_up: ~p~n", [Nick]),
    Server ! {self(), {chat, Msg}},
    io:fwrite("Chat request approved: ~p~n", [Nick]),
    case Count of
      0 -> io:fwrite("Chat done: ~p~n", [Nick]),
           MainId ! {self(), done};
      _ -> io:fwrite("Chatting again: ~p~n", [Nick]),
           chat_it_up(Server, MainId, Nick, Count - 1)
    end.