-module(setup_mqtt).
-export([init/0]).

init() ->
  Ssl_Options = [{verify, verify_none},
                 {versions, ['tlsv1.3', 'tlsv1.2']},
                 {ciphers, ssl:cipher_suites(all, 'tlsv1.3')}],
  {ok, Conn} = emqtt:start_link([{host, "pfff8f2e.us-east-1.emqx.cloud"},
                                 {port, 15818},
                                 {proto_ver, v5},
                                 {username, "ai_hydro1"},
                                 {password, "abcdefghi"},
                                 {ssl, true},
                                 {ssl_opts, Ssl_Options}]),
  {ok, _Cfg} = emqtt:connect(Conn),
  Topic = <<"IC.AIHydro/#">>,
  QoS = 2,
  {ok, _Props, _ReasonCodes} = emqtt:subscribe(Conn, {Topic, QoS}),
  mqtt_monitor:loop().

