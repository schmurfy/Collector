-module(collector_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, trace/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  io:format("[~w] Starting Collector.~n", [self()]),
  {ok, Pid} = collector_supervisor:start_link(_StartArgs),
  {ok, A} = application:get_env(adapters),
  start_adapters( A ),
  % collector_dispatcher:add_handler(collector_value_handler),
  {ok, Handlers} = application:get_env(handlers),
  start_handlers(Handlers),
  {ok, Pid}.

start_adapters([]) -> ok;
start_adapters([H|R]) ->
  collector_supervisor:start_adapter(H),
  start_adapters(R).

start_handlers([]) -> ok;
start_handlers([H|R]) ->
  collector_dispatcher:add_handler(alias, H).

stop(_State) ->
    ok.

trace(M) ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(M, '_', []),
  dbg:p(all,c).

% Called after code update
% config_changed(Changed, New, Removed) ->
%   ok.

