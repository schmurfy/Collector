-module (collector_presence_fsm_test).
-include_lib("eunit/include/eunit.hrl").

-include("collector.hrl").

main_test_() ->
  {foreach,
    fun setup/0,
    fun cleanup/1,
    [
      fun it_can_be_created/1,
      fun should_change_state_on_activity/1,
      fun should_refresh_last_activity_if_already_connected/1,
      fun should_change_to_disconnected_after_timeout/1
    ]
  }.

dummy_dispatcher() ->
  receive
    exit -> ok;
    _ -> dummy_dispatcher()
  end.

setup() ->
  {ok, Pid} = collector_presence_fsm:start_link("test_server", 100),
  % start a fake dispatcher
  spawn(fun() ->
    register(collector_dispatcher, self()),
    dummy_dispatcher()
  end),
  Pid.

cleanup(Pid) ->
  collector_dispatcher ! exit,
  collector_presence_fsm:stop(Pid).

it_can_be_created(Pid) ->
  fun() ->
    [State, Data] = collector_presence_fsm:get_state(Pid),
    ?assertEqual(disconnected, State),
    ?assertEqual(#checker_state{host = "test_server", timeout = 100, last_activity = nil}, Data)
  end.

should_change_state_on_activity(Pid) ->
  fun() ->
    gen_fsm:send_event(Pid, {activity, 12}),
    [State, Data] = collector_presence_fsm:get_state(Pid),
    ?assertEqual(connected, State),
    ?assertEqual(#checker_state{host = "test_server", timeout = 100, last_activity = 12}, Data)
  end.

should_refresh_last_activity_if_already_connected(Pid) ->
  fun() ->
    gen_fsm:send_event(Pid, {activity, 12}),
    [State, Data] = collector_presence_fsm:get_state(Pid),
    ?assertEqual(connected, State),
    ?assertEqual(#checker_state{host = "test_server", timeout = 100, last_activity = 12}, Data),
    
    gen_fsm:send_event(Pid, {activity, 23}),
    [State2, Data2] = collector_presence_fsm:get_state(Pid),
    ?assertEqual(connected, State2),
    ?assertEqual(#checker_state{host = "test_server", timeout = 100, last_activity = 23}, Data2)
  end.

should_change_to_disconnected_after_timeout(Pid) ->
  fun() ->
    gen_fsm:send_event(Pid, {activity, 12}),
    sleep(200),
    [State, _Data] = collector_presence_fsm:get_state(Pid),
    ?assertEqual(disconnected, State)
  end.


sleep(N) ->
  receive
    after N -> ok
  end.