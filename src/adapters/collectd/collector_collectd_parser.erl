-module (collector_collectd_parser).

-ifdef(TEST).
  -compile(export_all).
-else.
  -export([unpack/1]).
-endif.

-include("collector.hrl").

unpack(Binary) ->
  unpack(Binary, []).

unpack(<<>>, L) -> L;
unpack(Binary, L = []) ->
  {Rest, Packet} = parse_packet(Binary, #data{}),
  % io:format("Packet:~w~n", [?rec_info(data, Packet)]),
  unpack(Rest, [Packet|L]);
  
unpack(Binary, L = [H|_]) ->
  NewPacket = H#data{values = []},
  {Rest, Packet} = parse_packet(Binary, NewPacket),
  % io:format("Packet:~w~n", [?rec_info(data, Packet)]),
  unpack(Rest, [Packet|L]).


%% -----------
%% Parse packet
%% -----------

parse_packet(<<>>, Packet = #data{}) -> {<<>>, Packet};
parse_packet(Binary, #data{values = []} = Packet) ->
  {Rest, NewPacket} = parse_part(Binary, Packet),
  parse_packet(Rest, NewPacket);

% we have a complete packet
parse_packet(Binary, #data{} = Packet) ->
  NewPacket = Packet#data{values = lists:reverse(Packet#data.values)},
  {Binary, NewPacket}.


%% -----------
%% Parse part
%% -----------

parse_part(Binary, Packet) ->
  {PartID, Length, Rest1}   = parse_part_header(Binary),
  {NewPacket,  Rest2}       = parse_part_value(Rest1, PartID, Length, Packet),
  {Rest2, NewPacket}.
  

%% Unpacks part id and length from Binary. Return value is a triplet
%% of those plus the Binary remainder.
parse_part_header(Binary) ->
  <<PartID:16, Length:16, Rem/binary>> = Binary,
  % Length includes the 4 header bytes; we need to get rid of them.
  {PartID, Length - 4, Rem}.







%% Takes Length bytes from Binary and transforms them to an Erlang
%% expression using the part data type. The result is a tuple of
%% the updated data record and the Binary remainder.
parse_part_value(Binary, PartID, Length, Packet) ->
  <<Value:Length/binary-unit:8, Rest/binary>> = Binary,
  NewPacket = parse_part_value(PartID, Value, Packet),
  {NewPacket, Rest}.


parse_part_value(  0, Value, Packet) -> Packet#data{host             = parse_part_value(string,  Value)};
parse_part_value(  1, Value, Packet) -> Packet#data{time             = parse_part_value(numeric, Value)};
parse_part_value(  2, Value, Packet) -> Packet#data{plugin           = parse_part_value(string,  Value)};
parse_part_value(  3, Value, Packet) -> Packet#data{plugin_instance  = parse_part_value(string,  Value)};
parse_part_value(  4, Value, Packet) -> Packet#data{type             = parse_part_value(string,  Value)};
parse_part_value(  5, Value, Packet) -> Packet#data{type_instance    = parse_part_value(string,  Value)};
parse_part_value(  6, Value, Packet) -> Packet#data{values           = parse_part_value(values,  Value)};
parse_part_value(  7, Value, Packet) -> Packet#data{interval         = parse_part_value(numeric, Value)};
parse_part_value(100, Value, Packet) -> Packet#data{message          = parse_part_value(string,  Value)};
parse_part_value(101, Value, Packet) -> Packet#data{severity         = parse_part_value(numeric, Value)}.




parse_part_value(string, Value) ->
  % Remove null byte.
  Length = size(Value) - 1,
  <<String:Length/binary-unit:8, _:8>> = Value,
  % binary_to_list(String);
  String;
  
parse_part_value(numeric, <<Value:64>>) -> 
  Value;

parse_part_value(values, Value) ->
  <<Count:16, Rest/binary>> = Value,
  % Split the binary into types and values.
  {Types, Values} = split_binary(Rest, Count),
  parse_values(Count, Types, Values, []).


parse_values(0, <<>>, <<>>, Result) -> Result;
parse_values(Count, Types, Values, Result) ->
  % Extract the first type id and the first value.
  <<TypeID:8, RemTypes/binary>> = Types,
  <<Value:64/binary-unit:1, RemValues/binary>> = Values,
  parse_values(Count - 1, RemTypes, RemValues, [unpack_value(TypeID, Value)|Result]).

unpack_value(0, <<Value:64>>)         ->  {counter,   Value};
unpack_value(1, <<Value:8/bytes>>)    ->  {gauge,     ntohd(Value)};
unpack_value(2, <<Value:64>>)         ->  {derive,    Value};
unpack_value(3, <<Value:64>>)         ->  {absolute,  Value}.

ntohd(Value) ->
  binary_to_term(
    list_to_binary(
      [131, 70, lists:reverse(binary_to_list(Value))] ),
      [safe]
    ).


