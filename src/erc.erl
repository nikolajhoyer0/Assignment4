-module(erc).
-export([start/0,
         connect/2,
         chat/2,
         history/1,
         filter/3,
         get_filters/1,
         plunk/2,
         censor/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: A relay chat server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For staring an ERC server.
%   success => {ok, Server}
%   error   => {error, Reason}
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

connect(Server, _) when is_pid(Server) -> throw('connect: bad Nick');
connect(_, Nick) when is_atom(Nick)    -> throw('connect: bad Server');
connect(_, _)                          -> throw('connect: bad inputs').

% When a client is connected it should be ready to receive Erlang messages which
% are pairs of the form {Ref, Msg} where Ref is the reference returned from
% connect, and Msg is an ERC message, presumably for showing in some kind of UI.
% chat(Server, Cont) for sending a message with the content Cont, which should
% be a string, to all other clients in the room. This function should be
% non-blocking.
chat(Server, Cont) when is_pid(Server)
                   andalso is_list(Cont)
                   andalso length(Cont) > 0 ->
    % check that the message content is withing the valid range of extended
    % ASCII characters, i.e., 0 to 255.
    IsExtendedASCII = fun(Elem) ->
        erlang:is_integer(Elem) andalso (Elem > -1) andalso (Elem < 256) end,
    case lists:all(IsExtendedASCII, Cont) of
        true  -> async(Server, {chat, Cont});
        false -> throw('chat: message contains an invalid ASCII character')
    end;

chat(Server, _) when is_pid(Server) -> throw('chat: bad message');
chat(_, Cont) when is_list(Cont)    -> throw('chat: bad Server');
chat(_, _)                          -> throw('chat: bad inputs').


% history(Server) for getting the recent messages (capped at 42 messages) sent
% at the server. Returns a list of messages ordered so that newest message is
% the first element in the list and the last element is the oldest message.
history(Server) when is_pid(Server) ->
    blocking(Server, history);

history(_) -> throw('history: bad Server').

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
filter(Server, Method, Pred) when    is_pid(Server)
                             andalso is_atom(Method)
                             andalso is_function(Pred) ->

    case Method == compose orelse Method == replace of
       true  -> ok;
       false -> throw('filter: invalid method')
    end,

    try Pred({atom, "String"}) of
        true  -> ok;
        false -> ok;
        _     -> throw('filter: invalid predicate')
    catch _:_ -> throw('filter: invalid predicate')
    end,

    blocking(Server, {filter, Method, Pred});

filter(_, _, _) ->
    throw('filter: invalid inputs').

get_filters(Server) when is_pid(Server) ->
    blocking(Server, get_filters);

get_filters(_) -> throw('get_filters: bad Server').

% Adds a filter for ignoring any message from a Nick.
plunk(Server, Nick) when    is_pid(Server)
                    andalso is_atom(Nick) ->
    Pred = fun({PredNick, _}) ->
               case PredNick of
                   Nick -> false;
                   _    -> true
               end
           end,
    blocking(Server, {filter, compose, Pred});

plunk(Server, _) when is_pid(Server) -> throw('chat: bad Nick');
plunk(_, Nick) when is_atom(Nick)    -> throw('chat: bad Server');
plunk(_, _)                          -> throw('chat: bad inputs').

% censor(Server, Words) add a filter for ignoring messages containing any word
% in Words, which should be a list of strings. Should be implemented using
% filter with the compose method.
censor(Server, Words) when    is_pid(Server)
                      andalso is_list(Words) ->
    Pred = fun({_, Cont}) ->
        MemberIn = fun(Word) ->
            lists:member(Word, string:tokens(Cont, " "))
        end,
        not lists:any(MemberIn, Words)
    end,
    blocking(Server, {filter, compose, Pred});

censor(Server, _) when is_pid(Server) -> throw('chat: Words must be in a list');
censor(_, Words) when is_list(Words)  -> throw('chat: bad Server');
censor(_, _)                          -> throw('chat: bad inputs').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMUNICATION PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
blocking(Server, Request) ->
    Server ! {self(), Request},
    receive
        {Server, Response} -> Response
    end.

async(Server, Request) ->
    Server ! {self(), Request}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SERVER'S INTERNAL IMPLEMENTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ref       : unique reference generated by Erlang
% Clients   : {Client_Pid, Nick}
% Filters   : {Client_Pid, [Predicates]}
% MsgLog    : [Messages]
% Message   : {Nick, Content_String}
% Predicate : fun() -> boolean

loop(Ref, Clients, Filters, MsgLog) ->
    receive
        {Client, {connect, Nick}} ->
            NewClients = connect_logic(Client, Clients, Nick, Ref),
            loop(Ref, NewClients, Filters, MsgLog);

        {Client, {chat, Cont}} ->
            NewMsgLog = chat_logic(Client, Clients, Cont, Filters, Ref, MsgLog),
            loop(Ref, Clients, Filters, NewMsgLog);

        {Client, history} ->
            Client ! {self(), MsgLog},
            loop(Ref, Clients, Filters, MsgLog);

        {Client, {filter, Method, P}} ->
            NewFilters = filter_logic(Client, Filters, Method, P),
            Client ! {self(), {ok, updated_filter}},
            loop(Ref, Clients, NewFilters, MsgLog);

        {Client, get_filters} ->
            Client ! {self(), {ok, Filters}},
            loop(Ref, Clients, Filters, MsgLog);

        {Client, Other} ->
            Client ! {self(), {error, Other}},
            loop(Ref, Clients, Filters, MsgLog)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SERVER LOGIC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect_logic(Client, Clients, Nick, Ref) ->
    % check if the Nick is already taken by another user
    NickTaken = fun({_, N}) -> N == Nick end,
    case lists:any(NickTaken, Clients) of
        % is available, update Client list with new Nick
        false ->
            NewClients = [{Client, Nick} | Clients],
            Client ! {self(), {ok, Ref}};
        % is taken, reject
        true ->
            NewClients = Clients,
            Client ! {self(), {error, Nick, is_taken}}
    end,
    NewClients.

chat_logic(Client, Clients, Cont, Filters, Ref, MsgLog) ->

    % Check if the sender exists as a connected Client
    case lists:keyfind(Client, 1, Clients) of

        % Abort if the user does not exist
        false ->
            NewMsgLog = MsgLog;

        % Otherwise, proceed to send messages
        {_, FromNick} ->
            Msg = {FromNick, Cont},

            % Add the message to the log
            NewMsgLog = lists:sublist([ Msg | MsgLog ], 42),

            % A function for sending a single message to a client
            SendMsg = fun({ToPid, _}) ->
                case lists:keyfind(ToPid, 1, Filters) of

                    % No filter exists, just send the message
                    false ->
                        ToPid ! {Ref, Msg};

                    % One or more filters found. If any predicate returns
                    % false, do not send the message. Otherwise send it.
                    {_, Preds} ->
                        Allow = fun(Pred, Acc) -> Pred(Msg) and Acc end,
                        case lists:foldl(Allow, true, Preds) of
                            false -> ok;
                            true  -> ToPid ! {Ref, Msg}
                        end
                end
            end,

            % Send messages to all connected clients
            lists:foreach(SendMsg, Clients)
    end,
    NewMsgLog.

filter_logic(Client, Filters, Method, P) ->
    case lists:keyfind(Client, 1, Filters) of
        % client has no filters, add to list
        false ->
            NewFilters = [ {Client, [P] } | Filters ];
        % client has filters, check method and add new filter accordingly
        {Client, Ps} ->
            case Method of
                compose -> NewPs = [P | Ps];
                replace -> NewPs = [P]
            end,
            NewFilters = lists:keyreplace(Client, 1, Filters, {Client, NewPs})
    end,
    NewFilters.
