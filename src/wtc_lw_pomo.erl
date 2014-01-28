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
	 add_handler/2,
	 print/1,
	 notify/6]).

%% gen_event callbacks
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {percentDone=[], show=false}).

%%%===================================================================
%%% API
%%%===================================================================


%% @doc defines if pomo log prints to console or not
%% @end
-spec print(boolean()) -> ok.
print(Print) ->
	gen_event:call(?MODULE, ?MODULE, {set, show, Print}).

%% @doc notify all handlers about the new pomo state
%% @end
-spec notify(atom(), atom(), calendar:datetime(), calendar:datetime(), {calendar:day(), calendar:time()}, float()) -> ok.
notify(PomoName, PomoState, StartTime, EndTime, TimeLeft, PercentDone) ->
	gen_event:notify(?MODULE, {pomoState, PomoName, [{pomoState,PomoState}, {startTime,StartTime}, {endTime,EndTime}, {timeLeft,TimeLeft}, {percentDone,PercentDone}]}),
	ok.

%%%===================================================================
%%% gen_event callbacks
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
	Result = gen_event:start_link({local, ?MODULE}),
	gen_event:add_sup_handler(?MODULE, ?MODULE, []),
	Result.

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
	State = #state{},
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
handle_event({pomoState, _PomoName, _Props}, State) when State#state.show =:= false ->
	{ok, State};
handle_event({pomoState, PomoName, Props}, State) when State#state.show =:= true ->
	PercentDone = round(proplists:get_value(percentDone,Props)*100)/1,
	NewState = case proplists:get_value(PomoName,State#state.percentDone) of
		PercentDone ->
			State;
		_ ->
			PomoState = proplists:get_value(pomoState,Props),
			{_, {SH,SM,SS}} = proplists:get_value(startTime,Props),
			{_, {LH,LM,LS}} = proplists:get_value(timeLeft,Props),
			{_, {EH,EM,ES}} = proplists:get_value(endTime,Props),
			io:fwrite("~-18s ~5s - ~2B:~2B:~2B -> ~2B:~2B:~2B (~5.1f% done) -> ~2B:~2B:~2B~n",
				  [PomoName, PomoState, SH,SM,SS, LH,LM,LS, PercentDone, EH,EM,ES]),
			case proplists:is_defined(PomoName, State#state.percentDone) of
				true ->
					State#state{percentDone=lists:keyreplace(PomoName, 1, State#state.percentDone, {PomoName,PercentDone})};
				_ ->
					State#state{percentDone=[{PomoName,PercentDone}|State#state.percentDone]}
			end
	end,
	{ok, NewState};
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
handle_call({set, show, Show}, State) ->
	{ok, ok, State#state{show=Show}};
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


