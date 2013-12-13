%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtfs_chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 stop/1]).

-define(CONF(X), wtfs_chat_config:get(X, fun(K) -> throw({unable_to_get_configuration,K,X}) end)).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	application:ensure_started(httpStatusCodes),
	application:ensure_started(ranch),
	application:ensure_started(cowlib),
	application:ensure_started(cowboy),
	Dispatch = cowboy_router:compile([
		{'_', [	
			{"/", cowboy_static, {file, ?CONF([static,root])++"/index.html"}},
			{"/[...]", cowboy_static, {dir, ?CONF([static,root])}}
		]}
	]),
	case ?CONF([server,http,start]) of true ->
		lager:info("start http server"),
		{ok, _} = cowboy:start_http(http, ?CONF([server,http,acceptors]), [{port, ?CONF([server,http,port])}], [
			{env, [{dispatch, Dispatch}]},
			{onresponse, fun wtfs_chat_http_error:respond/4}
		])
	end,
	case ?CONF([server,https,start]) of true ->
		lager:info("start https server"),
		{ok, _} = cowboy:start_https(https, ?CONF([server,https,acceptors]), [
			{port, ?CONF([server,https,port])},
			{certfile, ?CONF([server,https,cert,file])}

		], [
			{env, [{dispatch, Dispatch}]},
			{onresponse, fun wtfs_chat_http_error:respond/4}
		])
	end,
	wtfs_chat_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


