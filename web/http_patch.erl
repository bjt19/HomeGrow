-module(http_patch).
-export([process/1]).
-include_lib("inets/include/httpd.hrl").

process(Mod) ->
  #{path := Uri} = uri_string:parse(string:to_lower(Mod#mod.request_uri)),
  Path = filename:split(Uri),
  case proplists:get_value("authorization", Mod#mod.parsed_header, none) of
    [$B,$a,$s,$i,$c,$ | Auth] ->
      case binary:split(base64:decode(Auth), [<<":">>]) of
        [User, Pass] ->
          io:fwrite("Got user/pass: ~p~n", [{User, Pass}]),
          ai_hydro:authorise(User, Pass, fun () -> route(Path, Mod, User) end);
        _ -> {401, <<"">>}
      end;
    _ -> {401, <<"">>}
  end.

route(["/", "system", SysId, "ownership"], Mod, User) ->
  gen_server:call({global, db_event_server}, {claim, User, iolist_to_binary(SysId)}).

