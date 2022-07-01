#!/usr/bin/sh
cd /beam
erlc /src/db/*.erl
erl -start-empd true -mnesia dir '"/mnesia/"' -name "database_mnesia@127.0.0.1" -run setup_mnesia init
