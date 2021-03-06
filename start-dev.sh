#!/bin/bash
APPNAME='wtc'
rebar update-deps compile
erl -name "${APPNAME}_${RANDOM}@data.mrpi" -pa ebin deps/*/ebin \
	-eval "application:start(lager)" \
	-eval "lager:set_loglevel(lager_console_backend,info)." \
	-eval "${APPNAME}:start()."
echo -e "\e[0;33mreturn value: \e[1;31m$?\e[0m"
