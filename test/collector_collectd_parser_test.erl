-module (collector_collectd_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("collector.hrl").


it_can_decode_numeric_part_test() ->
  Data = <<0, 1, 0, 12, 0, 0, 0, 0, 0, 0, 8, 252>>,
  {Rest, Packet} = collector_collectd_parser:parse_part(Data, #data{}),
  ?assertEqual(2300, Packet#data.time),
  ?assertEqual(<<>>, Rest).

it_can_decode_string_part_test() ->
  Data = <<0, 0, 0, 13, "hostname", 0>>,
  {Rest, Packet} = collector_collectd_parser:parse_part(Data, #data{}),
  ?assertEqual(<<"hostname">>, Packet#data.host),
  ?assertEqual(<<>>, Rest).

it_can_decode_value_part_test() ->
  Data = << 0, 6,  % Values type
            0, 42, % Size
            0, 4,  % Number of values
            0, 1, 2, 3, % Types
            0, 0, 0, 0, 0, 0, 8, 252,               % First value: counter(2300)
            86, 14, 45, 178, 245, 224, 224, 64,     % Second value: gauge(34567.678)
            0, 0, 0, 0, 2, 15, 117, 138,            % Third value: derive(-34567567)
            0, 0, 0, 0, 2, 15, 117, 138             % Fourth value: absolute(34567562)
            >>,
  {Rest, Packet} = collector_collectd_parser:parse_part(Data, #data{}),
  ?assertEqual({counter, 2300}, lists:nth(4, Packet#data.values)),
  ?assertEqual({gauge, 34567.678}, lists:nth(3, Packet#data.values)),
  ?assertEqual({derive, 34567562}, lists:nth(2, Packet#data.values)),
  ?assertEqual({absolute, 34567562}, lists:nth(1, Packet#data.values)),
  ?assertEqual(<<>>, Rest).


it_can_handle_invalid_packets_test() ->
  % 1 byte missing
  Data = <<0, 1, 0, 12, 0, 0, 0, 0, 0, 0, 8>>,
  ?assertError({badmatch, _}, collector_collectd_parser:unpack(Data)).
  