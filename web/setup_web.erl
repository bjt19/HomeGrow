-module(setup_web).
-export([init/0]).

-define(HTTP_PORT, 46971).
-define(HTTPS_PORT, 46972).

init() ->
  init_https().

init_https() ->
  ok = inets:start(),
  ok = ssl:start(),
  Ssl_Options = [
    {keyfile, "/etc/letsencrypt/live/1r.re/privkey.pem"},
    {certfile, "/etc/letsencrypt/live/1r.re/cert.pem"},
    {cacertfile, "/etc/letsencrypt/live/1r.re/chain.pem"},
    {verify, verify_peer},
    {versions, ['tlsv1.3', 'tlsv1.2']},
    {ciphers, ssl:cipher_suites(all, 'tlsv1.3')}
  ],
  Inets_Options = [
    {port, ?HTTPS_PORT},
    {keep_alive, true},
    {server_name, "ai-hydro-server"},
    {server_root, "/web"},
    {document_root, "/web/index"},
    % Uncomment this line to use HTTPS
    % {socket_type, {essl, Ssl_Options}},xz
    {modules, [web_callback]}
  ],
  {ok, Pid} = inets:start(httpd, Inets_Options),
  {ok, Pid}.

