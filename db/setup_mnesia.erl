-module(setup_mnesia).
-export([init/0, create/0]).
-include("/src/records.hrl").

init() ->
  ok = mnesia:create_schema([node()]),
  ok = mnesia:start(),
  ok = create(),
  halt(0).

create() ->
  {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)},
                                             {disc_copies, [node()]},
                                             {type, set}
                                            ]),
  {atomic, ok} = mnesia:create_table(system, [{attributes, record_info(fields, system)},
                                               {disc_copies, [node()]},
                                               {index, [#system.owner, #system.plant_id]},
                                               {type, set}
                                              ]),
  {atomic, ok} = mnesia:create_table(plant, [{attributes, record_info(fields, plant)},
                                             {disc_copies, [node()]},
                                             {type, set}
                                            ]),
  {atomic, ok} = mnesia:create_table(hydro_reading, [{attributes, record_info(fields, hydro_reading)},
                                                     {index, [#hydro_reading.timestamp, #hydro_reading.system]},
                                                     {disc_copies, [node()]},
                                                     {type, set}
                                                    ]),
  ok.
