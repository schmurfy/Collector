%%
%% Generate an event if a value was there but is now missing
%%
-module (collector_value_presence).

-behaviour(gen_event).

-include("collector.hrl").

-record (state, {
  procs = [],
  timeout = nil
}).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% Public API
-export ([list_connected/0]).

list_connected() ->
  Processes = gen_event:call(collector_dispatcher, ?MODULE, get_state),
  list_connected(Processes).

list_connected([]) -> ok;
list_connected([{Server, Pid}|Rest]) ->
  {State, #checker_state{last_activity = LastActivity}} = ask_fsm_state(Pid),
  io:format("~s: ~w (~w)~n", [Server, timestampToDate(LastActivity), State]),
  list_connected(Rest).

ask_fsm_state(Pid) ->
  collector_presence_fsm:get_state(Pid).

timestampToDate(Timestamp) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   Seconds       = BaseDate + Timestamp,
   calendar:gregorian_seconds_to_datetime(Seconds).



% Internal

init([Timeout]) ->
  io:format("[~w] Presence Handler installed (~w).~n", [self(), Timeout]),
  {ok, #state{timeout = Timeout}}.

handle_event({activity, #value_event{host = H, time = T}}, State = #state{timeout = Timeout, procs = P}) ->
  case lists:keyfind(H, 1, P) of
    false ->
      {ok, Pid} = collector_presence_fsm:start_link(H, Timeout),
      NewState = #state{procs = [{H, Pid}|P]};
      
    {H, Pid} ->
      NewState = State
  end,
  
  collector_presence_fsm:send_activity(Pid, H, T),
  {ok, NewState};

handle_event({server_connected, H}, State) ->
  io:format("Server connected: ~s~n", [H]),
  {ok, State};

handle_event({server_disconnected, H, _LastActivity}, State) ->
  io:format("Server lost: ~s~n", [H]),
  {ok, State};

% ignore other events
handle_event(_Event, State) ->
  {ok, State}.

handle_call(get_state, State) ->
  {ok, State, State};

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

