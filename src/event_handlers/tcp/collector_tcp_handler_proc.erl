-module(collector_tcp_handler_proc).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%%.
%%%'   PUBLIC API

start_link(Socket) ->
  % gen_server:start_link(?MODULE, [], []). % for unnamed gen_server
  gen_server:start_link(?MODULE, Socket, []).

% start() ->
%   gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%%.
%%%'   CALLBACKS

init(Socket) ->
  {ok, Socket}.

handle_call(_Req, _From, State) ->
  {reply, State}.


handle_cast({send_msg, Data}, Socket) ->
  gen_tcp:send(Socket, Data),
  {noreply, Socket};
  
handle_cast(stop, State) ->
  {stop, normal, State};
  
handle_cast(_Req, State) ->
  {noreply, State}.


handle_info({tcp, Socket, _Data}, Socket) ->
  io:format("[~w] TCP Handler received data, discarded~n", [self()]),
  inet:setopts(Socket, [{active, once}]),
  {noreply, Socket};

handle_info({tcp_closed, Socket}, Socket) ->
  io:format("[~w] TCP connection closed, terminating...~n", [self()]),
  {stop, normal, Socket};
  
handle_info(_Info, State) ->
  io:format("[~w] Debug: ~w state(~w)~n", [self(), State, _Info]),
  {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.


