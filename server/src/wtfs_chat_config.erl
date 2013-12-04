%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% read configuration from file
%%% 	1. "~/.wtfs_chat.conf" (if exist)
%%% 	2. "/etc/wtfs_chat.conf" (if "~/.wtfs_chat.conf" not exist)
%%% or uses defaults
%%% all options can be overwritten, by set application environment
%%% @end
%%%-------------------------------------------------------------------
-module(wtfs_chat_config).

-export([get/1]).

-include("defaults.hrl").


%% @doc get config value
%% @end
-spec get(term()) -> term().
get(Key) ->
	application:get_env(?APPLICATION,Key,get_conf(Key,?CONFIG_FILES)).
	


%% @doc replace all constants
%% @end
-spec replace_constants(string()) -> string().
replace_constants(Input) ->
	lists:foldl(fun(Constant,Acc) ->
			re:replace(Acc,"%%:"++Constant++":%%",
				   proplists:get_value(Constant,?CONSTANTS))
		    end, Input, proplists:get_keys(?CONSTANTS)).

%% @doc get property from object out of configuration file
%% @end
-spec get_conf_from(string(), term()) -> {ok, term()} | {error, atom()}.
get_conf_from(File, {Object, Property}) ->
	nyi.


%% @doc get value from config file
%% @end
-spec get_conf(term(), string()) -> term().
get_conf(_Key, []) ->
	{error, not_found};
get_conf(Key, [ConfFile|ConfigFiles]) ->
	case get_conf_from(ConfFile, Key) of
		{ok, Value} ->
			application:set_env(?APPLICATION, Key, Value),
			Value;
		_ ->
			get_conf(Key, ConfigFiles)
	end.
