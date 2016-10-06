-module(erc).
-export([start/0,
         connect/2,
         chat/2,
         history/1,
         filter/3,
         get_filters/1,
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
    Clients = [],
    Filters = [],
    MsgLog = [],
    try spawn(fun() -> loop(Ref, Clients, Filters, MsgLog) end) of
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

connect(_, _) -> throw('connect: bad input').


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
    blocking(Server, {filter, Method, P}).

get_filters(Server) ->
    blocking(Server, get_filters).

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
loop(Ref, Clients, Filters, MsgLog) ->
    receive
        {ClientId, {connect, Nick}} ->
            % check if the Nick is already taken by another user
            NickTaken = fun({_, N}) -> N == Nick end,
            case lists:any(NickTaken, Clients) of
                % is available, update Client list with new Nick
                false ->
                    NewClients = [{ClientId, Nick} | Clients],
                    ClientId ! {self(), {ok, Ref}},
                    loop(Ref, NewClients, Filters, MsgLog);
                % is taken, reject
                true ->
                    ClientId ! {self(), {error, Nick, is_taken}},
                    loop(Ref, Clients, Filters, MsgLog)
            end;

        {ClientId, {chat, Cont}} ->
            case lists:keyfind(ClientId, 1, Clients) of
                false ->
                    loop(Ref, Clients, Filters, MsgLog);
                {ClientId, Nick} ->
                    NewMsgLog = lists:sublist([ {Nick, Cont} | MsgLog ], 42),
                    SendMsg = fun({To, Nick_}) ->
                        To ! {Ref, {Nick_, Cont}} end,
                    lists:map(SendMsg, Clients),
                    loop(Ref, Clients, Filters, NewMsgLog)
            end;

        {ClientId, history} ->
            ClientId ! {self(), {ok, MsgLog}},
            loop(Ref, Clients, Filters, MsgLog);

        {ClientId, {filter, Method, P}} ->
            case lists:keyfind(ClientId, 1, Filters) of
                false ->
                    NewFilters = [ {ClientId, [P] } | Filters ],
                    ClientId ! {self(), {ok, 'filter: added for new client'}};
                {ClientId, Ps} ->
                    case Method of
                        compose ->
                            NewPs = [P | Ps];
                        replace ->
                            NewPs = [P];
                        _ ->
                            NewPs = Ps,
                            ClientId ! {self(), {error, 'filter: method invalid'}}
                    end,
                    NewFilters = lists:keyreplace(ClientId, 1, Filters, {ClientId, NewPs}),
                    ClientId ! {self(), {ok, 'filter: added for existing client'}};
                _ ->
                    NewFilters = Filters,
                    ClientId ! {self(), {error, 'filter: internal error'}}
            end,
            loop(Ref, Clients, NewFilters, MsgLog);

        {ClientId, get_filters} ->
            ClientId ! {self(), {ok, Filters}},
            loop(Ref, Clients, Filters, MsgLog);

        % {ClientId, {plunk, Nick}} ->
        %     loop(Ref, Clients, Filters, MsgLog);
        %
        % {ClientId, {censor, Words}} ->
        %     loop(Ref, Clients, Filters, MsgLog);

        {ClientId, Other} ->
            ClientId ! {self(), {error, Other}},
            loop(Ref, Clients, Filters, MsgLog)
    end.
