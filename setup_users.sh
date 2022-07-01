#!/usr/bin/sh

mkdir -p /mnesia
mkdir -p /web

adduser web-inets
chown -R web-inets /web
chmod -r 0744 /web

adduser db-mnesia
chown -R db-mnesia /mnesia
chmod -r 0744 /mnesia