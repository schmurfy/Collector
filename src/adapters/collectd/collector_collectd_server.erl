-module (collector_collectd_server).
-behaviour (gen_server).

-include("collector.hrl").

%% exported functions
-export ([start_link/1]).

%% internal exports
-export ([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export ([decode_data/1]).

start_link(Port) ->
  gen_server:start_link(?MODULE, [Port], []).
  

init([Port]) ->
  io:format("[~w] Collectd Server started on port ~w~n", [self(), Port]),
  {ok, _Socket} = gen_udp:open(Port, [binary]),
  {ok, nostate}.

  
handle_info({udp, _Socket, _ClientIP, _ClientPort, Packet}, State) ->
  process_flag(trap_exit, true),
  spawn_link(?MODULE, decode_data, [Packet]),
  {noreply, State};

handle_info({'EXIT', _Pid, {Reason, _Stack}}, State) ->
  io:format(" ~w~n", [Reason]),
  {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
  % io:format("Decoding completed: ~w~n", [Reason]),
  {noreply, State}.

decode_data(Data) ->
  Packets = collector_collectd_parser:unpack(Data),
  dispatch_events(Packets).

dispatch_events([]) -> ok;
dispatch_events([H = #data{message = nil}|Rest]) ->
  E1 = #value_event{
      host = H#data.host,
      time = H#data.time,
      interval = H#data.interval,
      values = H#data.values
    },
  
  E2 = translate_plugin(H, E1),
  E3 = translate_type(H, E2),
  collector_dispatcher:notify({activity, E3}),
  dispatch_events(Rest).

handle_call(_Req, _From, _State) -> ok.
handle_cast(_Req, _State) -> ok.


%% Helpers

% no plugin_instance
translate_plugin(#data{plugin = P, plugin_instance = nil}, Out = #value_event{}) ->
  Out#value_event{plugin = P};

translate_plugin(#data{plugin = P, plugin_instance = Pi}, Out = #value_event{}) ->
  NewPlugin = list_to_binary( lists:concat( [binary_to_list(P), '/', binary_to_list(Pi)] ) ),
  Out#value_event{plugin = NewPlugin}.


translate_type(#data{type = T, type_instance = nil}, Out = #value_event{}) ->
  Out#value_event{type = T};

translate_type(#data{type = T, type_instance = Ti}, Out = #value_event{}) ->
  NewType = list_to_binary( lists:concat( [binary_to_list(T), '/', binary_to_list(Ti)] ) ),
  Out#value_event{type = NewType}.





code_change(_OldVsn, State, _Extra) ->
  io:format("Collectd Server: code changed~n", []),
  {ok, State}.

terminate(_Reason, _State) ->
  io:format("Collectd Server exiting...~n", []).