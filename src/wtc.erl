%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc).

-export([]).


%% @doc start wtc
%% @end
-spec start() -> ok.
start() ->
	ok = application:ensure_started(lager),
	ok = application:ensure_started(httpStatusCodes),
	ok = application:ensure_started(crypto),
	ok = application:ensure_started(asn1),
	ok = application:ensure_started(public_key),
	ok = application:ensure_started(ssl),
	ok = application:ensure_started(ranch),
	ok = application:ensure_started(cowlib),
	ok = application:ensure_started(cowboy),
	application:start(wtc).

