-module(collector_tcp_handler_srv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2, accept_loop/1, send_msg/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%%.
%%%'   PUBLIC API

start_link(Port) ->
  io:format("[~w] TCP Handler Server installed.~n", [self()]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

% send a message to all connected sockets
send_msg(Msg) ->
  Children = supervisor:which_children(collector_tcp_handler_sup),
  send_msg(Msg, Children).

send_msg(_Msg, []) -> ok;
send_msg(Msg, [{_Id, Pid, _Type, _Modules}|Rest]) ->
  gen_server:cast(Pid, {send_msg, Msg}),
  send_msg(Msg, Rest).

% start() ->
%   gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%%.
%%%'   CALLBACKS

init(Port) ->
  {ok, Handle} = gen_tcp:listen(Port, [{active, once}, {reuseaddr, true}, binary]),
  spawn_link(?MODULE, accept_loop, [Handle]),
  {ok, Port}.

accept_loop(Handle) ->
  {ok, Socket} = gen_tcp:accept(Handle),
  collector_tcp_handler_sup:handle_connection(Socket),
  accept_loop(Handle).
  
handle_call(_Req, _From, State) ->
  {reply, State}.


handle_cast(stop, State) -> 
  {stop, normal, State};
  
handle_cast(_Req, State) ->
  {noreply, State}.


handle_info(_Info, State) -> 
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


