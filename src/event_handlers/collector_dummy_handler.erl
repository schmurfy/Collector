% For debug
% it prints all events received
-module (collector_dummy_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->
  io:format("Values Handler installed.~n", []),
  {ok, nostate}.

handle_event({activity, Event}, State) ->
  io:format("Event: ~w~n", [Event]),
  {ok, State}.


handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

