-module(db_event).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

-include("/src/records.hrl").

start_link() ->
  gen_server:start_link({global, db_event_server}, ?MODULE, [], []).

% gen server needs a state, we don't really
init(_Args) ->
  ok = mnesia:start(),
  io:fwrite("Started Mnesia~n"),
  ok = mnesia:wait_for_tables([user, system, hydro_reading], infinity),
  io:fwrite("Loaded Tables~n"),
  {atomic, Key} = mnesia:transaction(fun () -> mnesia:last(hydro_reading) end),
  K = if is_integer(Key) -> Key; true -> 0 end,
  {ok, #{reading_id => K}}.
run_if_owner(Id, User, Fun) when is_list(Id) ->
  run_if_owner(iolist_to_binary(Id), iolist_to_binary(User), Fun);

run_if_owner(Id, User, Fun) ->
  mnesia:transaction(fun () ->
    io:fwrite("Search for ~p~n", [Id]),
    case mnesia:read(system, Id) of
      [#system{owner=User}] ->
        io:fwrite("Found system owned by ~p~n", [User]),
        Fun();
      [] -> {error, no_sysid};
      S ->
        io:fwrite("Error, found ~p~n", [S]),
        {error, baduser}
    end
  end).

mk_hash(User, Pass) ->
  S0 = crypto:hash_init(blake2b),
  S1 = crypto:hash_update(S0, User),
  S2 = crypto:hash_update(S1, Pass),
  crypto:hash_final(S2).

get_latest(Mh, Gd, Name, Id, User) ->
  Rs = ['$$'],
  {atomic, Sel} = run_if_owner(Id, User, fun () ->
    mnesia:select(hydro_reading, [{Mh, Gd, Rs}])
  end),
  io:fwrite("Sel: ~p~n", [Sel]),
  Srt = lists:sort(fun ([TA|_], [TB|_]) -> TA > TB end, Sel),
  case Srt of
    [[_,L]|_] -> {ok, {[{Name, L}]}};
    _ -> {error, no_sysid}
  end.

handle_call({check_auth, {User, Pass}}, _From, State) ->
  {atomic, Matches} = mnesia:transaction(fun () -> mnesia:read(user, User) end),
  io:fwrite("Matches: ~p~n", [Matches]),
  Result = case Matches of
    [#user{user_id=User, auth=Auth}] ->
      Hash = crypto:hash_equals(Auth, mk_hash(User, Pass)),
      if Hash -> authorised;
         true -> not_authorised
      end;
    [] -> not_authorised
  end,
  {reply, Result, State};

% User Registration
handle_call({register, {User, Pass}}, _From, State) when is_binary(User) andalso is_binary(Pass) ->
  % Check if the user already exists
  Mh = #user{user_id=User, auth='$2'},
  Gd = [],
  {atomic, Rsp} = mnesia:transaction(fun () ->
    S = mnesia:select(user, [{Mh, Gd, [User,'$2']}]),
    if length(S) =/= 0 -> {error, name_in_use};
       true -> mnesia:write(#user{user_id=User, auth=mk_hash(User, Pass)})
    end
  end),
  {reply, Rsp, State};

handle_call({register, {User, Pass}}, From, State) ->
  handle_call({register, {list_to_binary(User), list_to_binary(Pass)}}, From, State);


% Single latest reading
handle_call({get_tds, #{id := Id, user := User}}, From, State) ->
  Mh = #hydro_reading{timestamp='$1', system=Id, dissolved_solids='$2', _ = '_'},
  Rsp = get_latest(Mh, [{'=/=', '$2', null}], tds, Id, User),
  {reply, Rsp, State};

handle_call({get_lightlevel, #{id := Id, user := User}}, From, State) ->
  Mh = #hydro_reading{timestamp='$1', system=Id, light_intensity='$2', _ = '_'},
  Rsp = get_latest(Mh, [{'=/=', '$2', null}], lightIntensity, Id, User),
  {reply, Rsp, State};

handle_call({get_waterlevel, #{id := Id, user := User}}, From, State) ->
  Mh = #hydro_reading{timestamp='$1', system=Id, water_ok='$2', _ = '_'},
  Rsp = get_latest(Mh, [{'=/=', '$2', null}], waterLevel, Id, User),
  {reply, Rsp, State};

handle_call({get_light_status, #{id := Id, user := User}}, From, State) ->
  Mh = #hydro_reading{timestamp='$1', system=Id, lights_on='$2', _ = '_'},
  Rsp = get_latest(Mh, [{'=/=', '$2', null}], lightsOn, Id, User),
  {reply, Rsp, State};

handle_call({get_pump_status, #{id := Id, user := User}}, From, State) ->
  Mh = #hydro_reading{timestamp='$1', system=Id, pump_on='$2', _ = '_'},
  Rsp = get_latest(Mh, [{'=/=', '$2', null}], pumpStatus, Id, User),
  {reply, Rsp, State};

% Multiple Readings
handle_call({get_data, #{since := S, before := B, count := C, id := Id, user := User}}, _From, State) ->
  Mh = #hydro_reading{reading_id='_',
                      timestamp='$1',
                      system=iolist_to_binary(Id),
                      dissolved_solids = '$2',
                      light_intensity = '$3',
                      water_ok = '$4',
                      lights_on = '$5',
                      pump_on = '$6'},
  io:fwrite("Check: Before ~B, after ~B~n", [B, S]),
  Gd = [{'>', B, '$1'}, {'<', S, '$1'}],
  Rs = ['$$'],
  {atomic, Sel} = run_if_owner(Id, User, fun () ->
    mnesia:select(hydro_reading, [{Mh, Gd, Rs}])
  end),
  Keys = mnesia:transaction(fun () -> mnesia:all_keys(hydro_reading) end),
  io:fwrite("~p~nKeys: ~p~n", [Sel, Keys]),
  Srt = lists:sort(fun ([TA|_], [TB|_]) -> TA > TB end, Sel),
  Sl = if C =:= all -> Srt; true -> lists:sublist(Srt, C) end,
  Rsp = {ok, [{[{timestamp, Ts},
                {dissolvedSolids, Ds},
                {lightIntensity, Li},
                {waterLevel, Wl},
                {lightsOn, Lo},
                {pumpOn, Po}]} || [Ts,Ds,Li,Wl,Lo,Po] <- Sl]},
  {reply, Rsp, State};

handle_call({claim, User, SysId}, _From, State) ->
  {atomic, Rsp} = mnesia:transaction(fun () ->
    case mnesia:wread({system, SysId}) of
      [S] when S#system.owner =:= null ->
        mnesia:write(S#system{owner = User}),
        {201, <<"">>};
      _ -> {409, <<"">>}
    end
  end),
  {reply, Rsp, State};


handle_call(Request, From, State) ->
  io:fwrite("Receive request: ~p~n", [Request]),
  {reply, {error, bad_request}, State}.

% Insert reading
handle_cast({reading, {light_level, Sn}, Reading}, State = #{reading_id := RIndex}) ->
  [A,B] = string:split(Reading, ",", all),
  {AInt, BInt} = {binary_to_integer(A), binary_to_integer(B) div 1200},
  {atomic, ok} = mnesia:transaction(fun () ->
    Len = length(mnesia:wread({system, Sn})),
    if Len =:= 0 -> mnesia:write(#system{sys_id=Sn}), io:fwrite("Added System ~p~n", [Sn]);
       true -> ok end,
    Rdx = #hydro_reading{reading_id=RIndex, system=Sn, timestamp=os:system_time(), light_intensity=[AInt, BInt]},
    io:fwrite("Writing ~p~n", [Rdx]),
    ok = mnesia:write(Rdx)
  end),
  {noreply, #{reading_id => RIndex + 1}};
handle_cast(Request, State) ->
  io:fwrite("Unknown cast: ~p~n", [Request]),
  {noreply, State}.

