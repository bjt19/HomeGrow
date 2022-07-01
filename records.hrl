



% Settings
-record(hydro_setting, {water_warning = 30,    % Warn after water level has been off for n seconds
                        nutrient_warning = 100, % Warn when nutrients fall below n ppm
                        light_intensity = (86400) div 5,  % How many equivalent seconds to keep the lights on per day
                        target_tds = 150        % What ppm to try and keep the tds at
                       }).
% Type of plant
-record(plant, {plant_id, % Int, Primary key
                plant_name, % String
                default_settings % #hydro_setting
               }).

% Data reading
-record(hydro_reading, {reading_id,       % Int, Primary Key
                        timestamp,        % Long Int
                        system,           % Int, Foreign Key (system)
                        dissolved_solids = null, % Nullable Int
                        light_intensity = null,  % Nullable Int
                        water_ok = null,         % Nullable Boolean
                        lights_on = null,        % Boolean
                        pump_on = null           % Boolean
                       }).

% System
-record(system, {sys_id,    % Int, Primary Key
                 plant_id = null,  % Nullable Int, Foreign Key (plant)
                 settings = #hydro_setting{},  % #hydro_setting
                 owner = null      % Nullable String, Foreign Key (user)
                }).

% User
-record(user, {user_id, % String/username, Primary Key
               auth     % String/hashed password
              }).

