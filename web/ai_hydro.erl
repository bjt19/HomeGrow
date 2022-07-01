-module(ai_hydro).
-export([authorise/3, system_request/3, user_request/3]).

-compile({inline, [call_server/1]}).
call_server(Msg) -> gen_server:call({global, db_event_server}, Msg).

authorise(User, Pass, OnSuccess) ->
  case call_server({check_auth, {User, Pass}}) of
    authorised ->
      io:fwrite("Authorised~n"),
      OnSuccess();
    _ -> {401, <<"">>}
  end.

system_req(data, Params) ->
  io:fwrite("Calling server: Get Data (~p)~n", [Params]),
  case call_server({get_data, Params}) of
    {ok, Data} -> {200, Data};
    {error, baduser} -> {403, <<"">>};
    {error, no_sysid} -> {404, <<"">>};
    {error, _} -> {400, <<"">>}
  end;

system_req(lightlevel, Params) ->
  io:fwrite("Calling server: Get Data (~p)~n", [Params]),
  case call_server({get_lightlevel, Params}) of
    {ok, Data} -> {200, Data};
    {error, baduser} -> {403, <<"">>};
    {error, no_sysid} -> {404, <<"">>};
    {error, _} -> {400, <<"">>}
  end;

system_req(waterlevel, Params) ->
  io:fwrite("Calling server: Get Data (~p)~n", [Params]),
  case call_server({get_waterlevel, Params}) of
    {ok, Data} -> {200, Data};
    {error, baduser} -> {403, <<"">>};
    {error, no_sysid} -> {404, <<"">>};
    {error, _} -> {400, <<"">>}
  end;
system_req(settings, Params) ->
  case call_server({get_settings, Params}) of 
    {ok, Settings} -> {200, {Settings}};
    {error, baduser} -> {403, <<"">>};
    {error, no_sysid} -> {404, <<"">>};
    {error, _} -> {400, <<"">>}
  end;

system_req(_Req, _Params) -> {400, <<"">>}.

system_request(SysId, Req, Params) ->
  io:fwrite("System Request: ~p, ~p, ~p~n", [SysId, Req, Params]),
  system_req(Req, Params#{id => iolist_to_binary(SysId)}).

user_request(User, register, #{pass := Pass}) ->
  io:fwrite("Register user ~p with password ~p~n", [User, Pass]),
  case gen_server:call({global, db_event_server}, {register, {User, Pass}}) of
    ok -> {201, <<"">>};
    {error, name_in_use} -> {409, <<"">>};
    {error, Err} ->
      io:fwrite("Error: ~p~n", [Err]),
      {400, <<"">>}
  end;

user_request(User, Req, Params) -> {501, <<"">>}.


