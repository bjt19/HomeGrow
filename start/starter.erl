-module(starter).
-export([start/0]).

-define(DB, 'database_mnesia@127.0.0.1').
-define(HTTP, 'http_backend@127.0.0.1').
-define(MQTT, 'mqtt@127.0.0.1').

start() ->
  pong = net_adm:ping(?DB),
  pong = net_adm:ping(?HTTP),
  pong = net_adm:ping(?MQTT),
  io:fwrite("Connected~n"),
  Web = spawn(?HTTP, setup_web, init, []),
  io:fwrite("Web initialised & running @~p on ~p~n", [Web, ?HTTP]),
  Db = spawn(?DB, application, start, [db_app]),
  io:fwrite("Db initialised & running @~p on ~p~n", [Db, ?DB]),
  Mqtt = spawn(?MQTT, setup_mqtt, init, []),
  io:fwrite("MQTT initialised & running @~p on ~p~n", [Mqtt, ?MQTT]),
  ok.
