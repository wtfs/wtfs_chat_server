%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	lager:debug("start link"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	lager:debug("init: Opts='[]'"),
	{ok, {{one_for_one, 5, 10}, [
		?CHILD(wtc_lw_pomo, wtc_lw_pomo, worker, []),
		?CHILD(wtc_mainServer, wtc_mainServer, worker, []),
		?CHILD(wtc_lw_pomo_2_1  , wtc_lw_pomo_prototype, worker, [ 2, 1]),
		?CHILD(wtc_lw_pomo_25_5 , wtc_lw_pomo_prototype, worker, [25, 5]),
		?CHILD(wtc_lw_pomo_32_8 , wtc_lw_pomo_prototype, worker, [32, 8]),
		?CHILD(wtc_lw_pomo_50_10, wtc_lw_pomo_prototype, worker, [50,10])
		]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


