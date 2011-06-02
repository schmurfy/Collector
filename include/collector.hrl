
%% Collectd packet structure

-record(data, {
  % Comments in each line are the corresponding part ids.
  host            = nil, %   0
  time            = nil, %   1
  plugin          = nil, %   2
  plugin_instance = nil, %   3
  type            = nil, %   4
  type_instance   = nil, %   5
  values          = [],  %   6
  interval        = nil, %   7
  message         = nil, % 100
  severity        = nil  % 101
}).


%% Internal event structure

-record(value_event, {
  host            = nil,
  time            = nil,
  plugin          = nil,
  type            = nil,
  values          = [],
  interval        = nil
}).

-record(message_event, {
  host            = nil,
  time            = nil,
  plugin          = nil,
  type            = nil,
  message         = nil
}).


% presence event handler
-define(PRESENCE_DELAY, 20000).


-record(checker_state, {
  host          = nil,
  timeout       = nil,
  last_activity = nil,
  timer         = nil
}).

