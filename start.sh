#!/usr/bin/env sh

id -u | /src/atoi

if [ $? -ne 0 ]; then
  return 13
fi

pkill -f beam.smp

cd /src/
erlc -o /beam/ /src/mqtt/*.erl /src/db/*.erl /src/web/*.erl /src/start/*.erl
if [ $? -ne 0 ]; then
  return 1
fi
cd /beam/

cookie=`tr -dc A-Za-z0-9 </dev/urandom | head -c 32 ; echo ''`

run_erlang() {
  runuser -l $ch_user -c "cd /beam && ERL_LIBS=/beam/_build/default/lib/ erl -start-empd true -noshell -name \"$sname@127.0.0.1\" -setcookie $cookie $extra_opts"&
}

ch_user=db-mnesia
sname=database_mnesia
extra_opts="-mnesia dir '\"/mnesia/\"'"
run_erlang

ch_user=web-inets
sname=http_backend
extra_opts=""
run_erlang

ch_user=web-inets
sname=mqtt
extra_opts=""
run_erlang

ERL_LIBS=/beam/_build/default/lib/ erl -noshell -name "starter@127.0.0.1" -setcookie $cookie -eval "starter:start()."
pkill -f beam.smp
return 0
