% Allow TCP Client to connect and send them bert formatted events:
%
% {connected, Host}                                     Host is now connected
% {server_disconnected, Host, LastActivity}             Host is now disconnected
% {data, {Host, Time, Plugin, Type, Values, Interval}}  Data update received
%   Values is a list of {DataType, DataValue}

-module(collector_tcp_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("collector.hrl").

-define (SUP_SPEC, {tcp_sup,
          {collector_tcp_handler_sup, start_link, []},
          permanent, 5000, worker, [collector_tcp_handler_sup]}).

-define (SRV_SPEC(Port), {tcp_srv,
          {collector_tcp_handler_srv, start_link, [Port]},
          permanent, 5000, worker, [collector_tcp_handler_srv]}).

% -record (state, {
%   port = nil
% }).

%%%.
%%%'   PUBLIC API


%%%.
%%%'   CALLBACKS
init([Port]) ->
  io:format("[~w] TCP Handler installed (Port: ~w).~n", [self(), Port]),
  self() ! {start, Port},
  {ok, []}.


handle_event({activity, #value_event{host = H, time = T, plugin = P, type = Type, values = V, interval = I}}, State) ->
  Data = bert:encode({data, {H, T, P, Type, V, I}}),
  collector_tcp_handler_srv:send_msg(Data),
  {ok, State};

handle_event({server_connected, Host}, State) ->
  Data = bert:encode({connected, Host}),
  collector_tcp_handler_srv:send_msg(Data),
  {ok, State};

handle_event({server_disconnected, Host, _LastActivity}, State) ->
  Data = bert:encode({disconnected, Host}),
  collector_tcp_handler_srv:send_msg(Data),
  {ok, State};

handle_event(_Message, State) ->
  {ok, State}.


handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.


handle_info({start, Port}, State) ->
  {ok, _Child1} = supervisor:start_child(collector_supervisor, ?SUP_SPEC),
  {ok, _Child2} = supervisor:start_child(collector_supervisor, ?SRV_SPEC(Port)),
  {ok, State};
  
handle_info(_Info, State) ->
  {ok, State}.


terminate(_Args, Port) ->
  io:format("[~w] TCP Handler terminated (Port: ~w).~n", [self(), Port]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%.
%%%'   PRIVATE FUNCTIONS

