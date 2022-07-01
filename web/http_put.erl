-module(http_put).
-export([process/1]).
-include_lib("inets/include/httpd.hrl").


process(Mod=#mod{request_uri="/user/register", parsed_header=Header}) ->
  User = proplists:get_value("username", Header, none),
  Pass = proplists:get_value("password", Header, none),
  if User =:= none orelse Pass =:= none -> {400, <<"">>};
     true -> ai_hydro:user_request(User, register, #{pass => Pass})
  end;

process(_Mod) -> {404, <<"">>}.
