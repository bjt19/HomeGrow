-module(http_get).
-export([process/1]).
-include_lib("inets/include/httpd.hrl").

process(Mod) ->
  {Query, Path} = case uri_string:parse(string:to_lower(Mod#mod.request_uri)) of
    #{path := Uri, query := Qstr} -> {uri_string:dissect_query(Qstr), filename:split(Uri)};
    #{path := Uri} -> {[], filename:split(Uri)}
  end,
  case proplists:get_value("authorization", Mod#mod.parsed_header, none) of
    [$B,$a,$s,$i,$c,$ | Auth] ->
      case binary:split(base64:decode(Auth), [<<":">>]) of
        [User, Pass] ->
          io:fwrite("Got user/pass: ~p~n", [{User, Pass}]),
          ai_hydro:authorise(User, Pass, fun () -> route(Path, Query, Mod, User) end);
        _ -> {401, <<"">>}
      end;
    _ -> {401, <<"">>}
  end.

validate_get_data(S, B, C) when is_list(S) ->
  case string:to_integer(S) of
    {X, []} -> validate_get_data(X, B, C);
    _ -> {400, #{invalid => #{since => S}}}
  end;
validate_get_data(S, B, C) when is_list(B) ->
  case string:to_integer(B) of
    {X, []} -> validate_get_data(S, X, C);
    _ -> {400, #{invalid => #{before => B}}}
  end;
validate_get_data(S, B, "all") -> {S, B, all};
validate_get_data(S, B, C) when is_list(C) ->
  case string:to_integer(C) of
    {X, []} -> {S, B, X};
    _ -> {400, #{invalid => #{count => C}}}
  end;
validate_get_data(_,_,_) -> {400, <<"Bad Format">>}.

route(["/", "system", SysId, "data"], Query, Mod, User) ->
  Since = proplists:get_value("since", Query, "0"),
  Before = proplists:get_value("before", Query, "18446744073709551615"),
  Count = proplists:get_value("count", Query, "all"),
  case validate_get_data(Since, Before, Count) of
    {S, B, C} ->
      R = ai_hydro:system_request(SysId, data, #{since => S, before => B, count => C, user => User}),
      io:fwrite("result: ~p~n", [R]),
      R;
    Error -> Error
  end;

route(["/", "system", SysId, Req], Query, Mod, User) ->
  ai_hydro:system_request(SysId, list_to_atom(Req), #{user => User});


route(Path, _Query, _Mod, _User) ->
  io:fwrite("Can't route~p~n", [Path]),
  {400, <<"Error">>}.

