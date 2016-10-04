-module(erc).
-export([start/0,
         connect/2,
         chat/2,
         history/1,
         filter/3,
         plunk/2,
         censor/2]).

% A relay chat server API in erlang (ERC).

% For starting an ERC server.
% Returns {ok, Server} on success or {error, Reason} if an error occurs.
start() -> spawn(fun loop/0).

% Just a general template right now... does not do anything really.
loop() ->
  receive
    {From, Request} ->
      From ! {self(), ComputeResult(Request)},
      loop();

    {From, Other} ->
      From ! {self(), {error, Other}},
      loop()
  end.



%connect(Serve, Nick).
%chat(Server, Cont).
%history(Server).
%filter(Sever, Method, P).
%plunk(Server, Nick).
%censor(Server, Words).