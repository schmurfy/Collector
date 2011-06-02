-module (collector_presence_fsm).

-behaviour (gen_fsm).

-include("collector.hrl").

-ifdef(TEST).
  -compile(export_all).
-endif.

-export ([start_link/2, init/1, connected/2, disconnected/2, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export ([send_activity/3, get_state/1]).

start_link(Host, Timeout) ->
  gen_fsm:start_link(?MODULE, [Host, Timeout], []).


% Public api
send_activity(Pid, _Host, Time) ->
  gen_fsm:send_event(Pid, {activity, Time}).

% timeout is the time after which the machine is considered
% disconnected.

init([Host, Timeout]) ->
  io:format("[~w] Presence monitor spawned for ~s / ~w ~n", [self(), Host, Timeout]),
  {ok, disconnected, #checker_state{host = Host, timeout = Timeout}}.


connected({timeout, _Ref, _Msg}, StateData = #checker_state{host = H, last_activity = Act}) ->
  % io:format("Server disconnected ~s ~n", [H]),
  collector_dispatcher:notify({server_disconnected, H, Act}),
  {next_state, disconnected, StateData};

% some data was received for this machine
% go in connected state.

connected({activity, Time}, StateData = #checker_state{timeout = T, timer = OldTimer}) ->
  % io:format("Activity received ~s ~n", [StateData#checker_state.host]),
  _Rest = gen_fsm:cancel_timer(OldTimer),
  Timer = gen_fsm:start_timer(T, nomsg),
  {next_state, connected, StateData#checker_state{last_activity = Time, timer = Timer}}.

disconnected({activity, Time}, StateData = #checker_state{host = H, timeout = T}) ->
  % io:format("Server connected ~s ~n", [H]),
  Timer = gen_fsm:start_timer(T, nomsg),
  collector_dispatcher:notify({server_connected, H}),
  {next_state, connected, StateData#checker_state{last_activity = Time, timer = Timer}}.

handle_event(_Event, StateName, StateData) ->
  io:format("Unknown event: ~w ~n", [_Event]),
  {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

% Testing interface
handle_sync_event(return_state, _From, StateName, StateData) ->
  {reply, {StateName, StateData}, StateName, StateData};
handle_sync_event(stop, _From, StateName, StateData) ->
  {stop, normal, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ok, StateName, StateData}.

get_state(Pid) -> gen_fsm:sync_send_all_state_event(Pid, return_state).

-ifdef(TEST).
stop(Pid) -> gen_fsm:sync_send_all_state_event(Pid, stop).
-endif.




