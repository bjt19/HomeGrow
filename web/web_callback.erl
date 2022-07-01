-module(web_callback).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

mk_body(Txt) -> binary_to_list(Txt).
mk_header(Code, Type, Txt) ->
   [{code, Code},
    {content_length, integer_to_list(byte_size(Txt))},
    {content_type, Type},
    {server, "inets/8.0"},
    {date, httpd_util:rfc1123_date()}].

format({Code, Response}) when is_integer(Code) ->
  io:fwrite("Respond ~p / ~p\r\n", [Code, Response]),
  {Type, Txt} = if
    is_binary(Response) -> {"text/plain", Response};
    true ->
      Json = iolist_to_binary(jiffy:encode(Response)),
      io:fwrite("~p~n", [Json]),
      {"application/json", Json}
  end,
  {break, [{response, {response, mk_header(Code, Type, Txt), mk_body(Txt)}}]}.

do(Mod) ->
  Response = route(Mod),
  format(Response).

route(Mod=#mod{method="GET"}) -> http_get:process(Mod);
route(Mod=#mod{method="HEAD"}) -> http_head(Mod);
route(Mod=#mod{method="PUT"}) -> http_put:process(Mod);
route(Mod=#mod{method="PATCH"}) -> http_patch:process(Mod);
route(Mod=#mod{method="POST"}) -> http_post(Mod);
route(Mod=#mod{method="DELETE"}) -> http_delete(Mod);
route(Mod=#mod{method="OPTIONS"}) -> http_options(Mod);
route(Mod) ->
  #mod{data = _Data,
       socket_type = _Socket_Type,
       socket = _Socket,
       config_db = _Config_Db,
       method = Method,
       absolute_uri = _Absolute_Uri,
       request_uri = _Request_Uri,
       http_version = _Http_Version,
       request_line = _Request_Line,
       parsed_header = _Parsed_Header,
       entity_body = _Entity_Body,
       connection = _Connection
      } = Mod,
  R = lists:flatten(io_lib:format("Unsupported: ~s~n", [Method])),
  {proceed, [{response, {405, R}}]}.

http_head(Mod) ->
  {proceed, [{response, {501, ""}}]}.

http_post(Mod) ->
  {proceed, [{response, {501, ""}}]}.

http_put(Mod) ->
  {proceed, [{response, {501, ""}}]}.

http_delete(Mod) ->
  {proceed, [{response, {501, ""}}]}.

http_patch(Mod) ->
  {proceed, [{response, {501, ""}}]}.

http_options(Mod) ->
  {proceed, [{response, {501, ""}}]}.

