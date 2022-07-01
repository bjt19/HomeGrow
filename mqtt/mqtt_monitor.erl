-module(mqtt_monitor).
-export([loop/0]).

loop() ->
  io:fwrite("Receiving~n"),
  receive
    {publish, #{topic := Topic, payload := Payload}} ->
      Msg = match_topic(Topic),
      gen_server:cast({global, db_event_server}, {reading, Msg, Payload}),
      io:fwrite("Got ~p from ~p~n", [Payload, Topic]);
    _ -> ok
  end,
  loop().

match_topic(Topic) ->
  case string:split(Topic, <<"/">>, all) of
    [<<"IC.AIHydro">>, Sn, <<"LightLevel">>] -> {light_level, Sn};
    [<<"IC.AIHydro">>, Sn, <<"TDS">>] -> {tds, Sn};
    [<<"IC.AIHydro">>, Sn, <<"WaterLevel">>] -> {water_level, Sn};
    [<<"IC.AIHydro">>, Sn, <<"LightOn">>] -> {light_on, Sn};
    [<<"IC.AIHydro">>, Sn, <<"PumpOn">>] -> {pump_on, Sn}
  end.

