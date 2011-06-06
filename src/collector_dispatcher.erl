% Possible events:
% - activity:             #value_event{}
%                         #message_event{}
%
% - server_disconnected:  hostname, lastseen
% - server_connected:     hostname)

-module (collector_dispatcher).

-include("collector.hrl").

-export([start_link/0, add_handler/2, add_sup_handler/2, notify/1]).

start_link() ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  io:format("[~w] Event dispatcher started.~n", [Pid]),
  {ok, Pid}.


add_handler(alias, dummy) -> add_sup_handler(collector_dummy_handler, []);
add_handler(alias, {tcp, ListenPort}) -> add_sup_handler(collector_tcp_handler, [ListenPort]);
add_handler(alias, {presence, Timeout}) -> add_sup_handler(collector_value_presence, [Timeout]);

add_handler(Module, Args) -> gen_event:add_handler(?MODULE, Module, Args).
add_sup_handler(Module, Args) -> gen_event:add_handler(?MODULE, Module, Args).


notify(Event) ->
  gen_event:notify(?MODULE, Event).





