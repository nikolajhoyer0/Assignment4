-module(erc).
-export([start/0,
         connect/2,
         chat/2,
         history/1,
         filter/3,
         plunk/2,
         censor/2]).

% Objective

% This assignment is about implementing a relay chat server in Erlang (ERC). A
% relay chat server consists of a central server that clients can connect to,
% and then send messages which are relayed (broadcast) to other connected
% clients. An often seen problem with relay chat servers is that when the number
% of users goes up, various mechanisms are needed for reducing the noise (the
% number of messages) in the network, the most basic one is the notion of
% filters.

% The server will keep track of which clients are connected, and for each
% client, possibly, a filter. When a client sends a message, it is relayed to
% other connected clients. Each client is registered with a nickname.

% A message is a pair {Nick, Cont} where Nick is the nickname of the client who
% sent the message, and Cont is the content of the message.

%%%
%%% API: A relay chat server.
%%%

% start() for staring an ERC server. Returns {ok, Server} on success or
% {error, Reason} if some error occurred.
start() ->
    Ref = make_ref(),
    Nicks = [],
    MsgLog = [],
    try spawn(fun() -> loop(Ref, Nicks, MsgLog) end) of
        Server -> {ok, Server}
    catch
        _:_ -> {error, this_should_not_happen}
    end.

% connect(Server, Nick) for connecting to an ERC server, with the nickname Nick
% which should be an atom. Returns {ok, Ref} and adds the client to the server
% if no other client is connected using that nickname. Ref is a unique reference
% used for identifying messages from the server to the client. Otherwise, if the
% nickname Nick is taken it should return {error, Nick, is_taken}.

connect(Server, Nick) when is_pid(Server) andalso is_atom(Nick) ->
    blocking(Server, {connect, Nick});

connect(Server, _) when is_pid(Server) -> throw('connect: bad Nick');
connect(_, Nick) when is_atom(Nick)    -> throw('connect: bad Server');
connect(_, _)                          -> throw('connect: bad inputs').


% When a client is connected it should be ready to receive Erlang messages which
% are pairs of the form {Ref, Msg} where Ref is the reference returned from
% connect, and Msg is an ERC message, presumably for showing in some kind of UI.

% chat(Server, Cont) for sending a message with the content Cont, which should
% be a string, to all other clients in the room. This function should be
% non-blocking.
chat(Server, Cont) when is_list(Cont) ->
    async(Server, {chat, Cont});

chat(_, _) -> throw('chat: bad input').


% history(Server) for getting the recent messages (capped at 42 messages) sent
% at the server. Returns a list of messages ordered so that newest message is
% the first element in the list and the last element is the oldest message.
history(Server) ->
    blocking(Server, history).

% filter(Server, Method, P) for filtering messages before they are sent to the
% client. Where:

%  (a)  P is predicate that takes a message (that is, a tuple of an atom and a
%       string) and returns true if the message should be sent to the client,
%       or false if the message should not be sent to the client;

%  (b)  Method is one of the atoms compose or replace. If Method is compose it
%       means that P should be composed with the previous filter, Q, (if any)
%       installed for the client, meaning that both P and Q must return true
%       for a message to be sent to the client. Otherwise, P should replace
%       any previous filter (if any) installed for the client.
filter(Server, Method, P) ->
    async(Server, {filter, Method, P}).

% plunk(Server, Nick) add a filter for ignoring any message from Nick. Should
% be implemented using filter, with the compose method.
plunk(Server, Nick) ->
    async(Server, {plunk, Nick}).

% censor(Server, Words) add a filter for ignoring messages containing any word
% in Words, which should be a list of strings. Should be implemented using
% filter with the compose method.
censor(Server, Words) ->
    async(Server, {censor, Words}).



%%%
%%% COMMUNICATION PRIMITIVES
%%%
blocking(Server, Request) ->
    Server ! {self(), Request},
    receive
        {Server, Response} -> Response
    end.

async(Server, Request) ->
    Server ! {self(), Request}.



%%%
%%% SERVER'S INTERNAL IMPLEMENTATION
%%%
loop(Ref, Nicks, MsgLog) ->
    receive
        {From, {connect, Nick}} ->
            case lists:keyfind(Nick, 2, Nicks) of
                false ->
                    NewNicks = [{From, Nick}|Nicks],
                    From ! {self(), {ok, Ref}},
                    loop(Ref, NewNicks, MsgLog);
                _ ->
                    From ! {self(), {error, Nick, is_taken}},
                    loop(Ref, Nicks, MsgLog)
            end;

        {From, {chat, Cont}} ->
            case lists:keyfind(From, 1, Nicks) of
                false ->
                    loop(Ref, Nicks, MsgLog);
                {From, Nick} ->
                    NewMsgLog = lists:sublist([ {Nick, Cont} | MsgLog ], 42),
                    SendMsg = fun({To, Nick_}) -> To ! {Ref, {Nick_, Cont}} end,
                    lists:map(SendMsg, Nicks),
                    loop(Ref, Nicks, NewMsgLog)
            end;

        {From, history} ->
            From ! {self(), {ok, MsgLog}},
            loop(Ref, Nicks, MsgLog);

        % {From, {filter, Method, P}} ->
        %     loop(Ref, Nicks, MsgLog);
        %
        % {From, {plunk, Nick}} ->
        %     loop(Ref, Nicks, MsgLog);
        %
        % {From, {censor, Words}} ->
        %     loop(Ref, Nicks, MsgLog);

        {From, Other} ->
            From ! {self(), {error, Other}},
            loop(Ref, Nicks, MsgLog)
    end.
