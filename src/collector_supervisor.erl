
%% Application Supervisor
-module(collector_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1, start_adapter/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

start_adapter({collectd, Port}) ->
  Spec = {collectd_adapter,
            {collector_collectd_server, start_link, [Port]},
            permanent, 5000, worker, [collector_collectd_server]},
  {ok, _Pid} = supervisor:start_child(?MODULE, Spec);

start_adapter(Spec) ->
  io:format("Unknown Adapater: ~w~n", [Spec]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("[~w] Starting supervisor.~n", [self()]),
    {ok, {{one_for_one, 5, 10},
      [
        ?CHILD(collector_dispatcher, worker, [])
      ]
    }}.

