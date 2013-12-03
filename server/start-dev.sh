#!/bin/bash
APPNAME='wtfs_chat'
rebar compile
erl -name "${APPNAME}_${RANDOM}@data.mrpi" -pa ebin deps/*/ebin \
	-eval "application:start(lager)" \
	-eval "lager:set_loglevel(lager_console_backend,debug)." \
	-eval "{ok, RIn} = file:get_cwd(), Rout = re:replace(RIn,\"/server\",\"/client\",[{return,list}]), application:set_env($APPNAME, root, Rout), io:format(\"www root is set to: ~p~n\", [Rout])." \
	-eval "application:start($APPNAME)."
echo -e "\e[0;33mreturn value: \e[1;31m$?\e[0m"
