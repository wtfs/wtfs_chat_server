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

-export([get/1,get/2]).

-include("defaults.hrl").



%% @doc 
%% @end
-spec get(term(), term()) -> term().
get(Key, Default) ->
	case ?MODULE:get(Key) of
		{ok, Value} -> Value;
		_ -> Default
	end.

%% @doc get config value
%% @end
-spec get(term()) -> term().
get(Key) ->
	case application:get_env(?APPLICATION,Key) of
		{ok, Value} -> {ok, Value};
		_ -> get_conf(Key,?CONFIG_FILES)
	end.
	


%% @doc replace all constants
%% @end
-spec replace_constants(string()) -> string().
replace_constants(Input) ->
	Constants = ?CONSTANTS,
	lists:foldl(fun(Constant,Acc) ->
			re:replace(Acc,"%%%:"++Constant++":%%%",
				   proplists:get_value(Constant,Constants), [{return,list}])
		    end, Input, proplists:get_keys(Constants)).


%% @doc extract value from config
%% @end
-spec extract_key([term()],[term()]) -> {ok, term()} | {error, atom()}.
extract_key(Data, []) ->
	case io_lib:printable_list(Data) of
		true -> {ok, replace_constants(Data)};
		false -> {ok, Data}
	end;
extract_key(Data, [Key|SubKey]) ->
	case proplists:is_defined(Key,Data) of
		true -> extract_key(proplists:get_value(Key, Data), SubKey);
		false -> {error, not_found}
	end.


%% @doc get property from object out of configuration file
%% @end
-spec get_conf_from(string(), [term()]) -> {ok, term()} | {error, atom()}.
get_conf_from(File, Key) ->
	case file:consult(File) of
		{ok, Data} ->
			extract_key(Data, Key);
		Error ->
			Error
	end.


%% @doc get value from config file
%% @end
-spec get_conf([term()], [string()]) -> {ok, term()} | {error, atom()}.
get_conf(_Key, []) ->
	{error, not_found};
get_conf(Key, [ConfFile|ConfigFiles]) ->
	case get_conf_from(ConfFile, Key) of
		{ok, Value} ->
			application:set_env(?APPLICATION, Key, Value),
			{ok, Value};
		_ ->
			get_conf(Key, ConfigFiles)
	end.
