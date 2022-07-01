#!/usr/bin/sh
mkdir -p /beam
mkdir -p /mnesia
cd /beam
erlc /src/db/*.erl
erl -start-empd true -mnesia dir '"/mnesia/"' -name "database_mnesia@127.0.0.1" -run setup_mnesia init
