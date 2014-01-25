%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2014 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc_lw_pomo).

-behaviour(gen_event).

%% API
-export([start_link/0,
	 add_handler/2]).

%% gen_event callbacks
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(pomo, {name="", work=0, break=0, timestamp={0,0,0}, current=init}).
-record(state, {timer, pomos=[]}).

%%%===================================================================
%%% gen_event API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	lager:debug("start link"),
	Res = gen_event:start_link({local, ?MODULE}),
	gen_event:add_sup_handler(?MODULE, ?MODULE, []),
	Res.

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler(Handler, Args) -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
	lager:debug("add handler: Handler:'~p', Args='~p'", [Handler, Args]),
	gen_event:add_sup_handler(?MODULE, Handler, Args).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	lager:debug("init: Opts='[]'"),
	{ok,{interval,Timer}} = timer:send_interval(1000,tick),
	Pomo = #pomo{name="2/1", work=10, break=5},
	State = #state{timer=Timer, pomos=[Pomo]},
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(Event, State) ->
	lager:warning("unexpected event: Event='~p', State='~p'", [Event, lager:pr(State,?MODULE)]),
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, State) ->
	lager:warning("unexpected call: Request='~p', State='~p'", [Request, lager:pr(State,?MODULE)]),
	Reply = unexpected,
	{ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(tick, State) ->
	NewState = State#state{pomos=[ calc_pomo(Pomo) || Pomo <- State#state.pomos ]},
	{ok, NewState};
handle_info(Info, State) ->
	lager:warning("unexpected info: Info='~p', State='~p'", [Info, lager:pr(State,?MODULE)]),
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	lager:debug("terminate: Reason='~p', State='~p'", [Reason, lager:pr(State,?MODULE)]),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	lager:notice("code change: OldVsn='~p', State='~p', Extra='~p'", [OldVsn, lager:pr(State,?MODULE), Extra]),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @doc calculate and sends the given pomo to all handler
%% @end
-spec calc_pomo(#pomo{}) -> #pomo{}.
calc_pomo(Pomo) ->
	case round(timer:now_diff(erlang:now(), Pomo#pomo.timestamp)/1000000) of
		Diff when Diff =:= Pomo#pomo.work -> %break stats just now
			io:format("~nbreak start~n"),
			io:format("<[~p]", [Diff]),
			Pomo;
		Diff when Diff < Pomo#pomo.work -> %it's time to work
			io:format(".[~p]", [Diff]),
			Pomo;
		Diff when Diff >= (Pomo#pomo.work+Pomo#pomo.break) -> %a new pomo stats just now
			io:format("~npomo start~n"),
			io:format("<[~p]", [Diff]),
			Pomo#pomo{timestamp=erlang:now()};
		Diff when Diff > Pomo#pomo.work -> %we are in break
			io:format(",[~p]", [Diff]),
			Pomo;
		Diff ->
			lager:error("unexpected pomo state, Diff is ~p @ Pomo: ~p", [Diff, Pomo]),
			Pomo
	end.
	
%% @doc convert erlang now timestamp, pomo/break duration
%% to a term, includes start time and date, end time and date, process, left duration
%% @end
-spec calc_pomo_start_end_process(tuple(), non_neg_integer(), non_neg_integer()) -> tuple().
calc_pomo_start_end_process(TimeStamp, Duration, Elapsed) ->
	Start = calendar:now_to_universal_time(TimeStamp),
	End = calendar:gregorian_seconds_to_datetime(
		calendar:datetime_to_gregorian_seconds(TimeStamp) +
		Duration
	       ),
	{Start,End}.

