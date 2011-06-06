%
% Supervise the process connecting to tcp clients
% it also serves a TCP server
%

-module (collector_tcp_handler_sup).

-behaviour (supervisor).

-export ([start_link/0, handle_connection/1, init/1]).



start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  io:format("[~w] TCP Handler Supervisor installed.~n", [Pid]),
  {ok, Pid}.
  

handle_connection(Socket) ->
  {ok, Child} = supervisor:start_child(?MODULE, [Socket]),
  % transfer socket ownership
  gen_tcp:controlling_process(Socket, Child).

init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    {tcp_handlers,
      {collector_tcp_handler_proc, start_link, []},
      transient,  % do not restart on normal exit
      5000,
      worker,
      [collector_tcp_handler_proc]}
  ]
  }}.