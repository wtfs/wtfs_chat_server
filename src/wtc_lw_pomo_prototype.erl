%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2014 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc_lw_pomo_prototype).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
	 work/2,
	 work/3,
	 break/2,
	 break/3,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-record(state, {pomo_name, workTime=0, breakTime=0, startTime={0,0,0}, endTime={0,0,0}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(WorkTime, BreakTime) ->
	lager:debug("WorkTime='~p', BreakTime='~p'", [WorkTime, BreakTime]),
	Name = erlang:list_to_atom("wtc_lw_pomo_"++erlang:integer_to_list(WorkTime)++"_"++erlang:integer_to_list(BreakTime)),
	gen_fsm:start_link({local, Name}, ?MODULE, {WorkTime*60, BreakTime*60, Name}, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init({WorkTime, BreakTime, Name}) ->
	lager:debug("WorkTime='~p', BreakTime='~p'", [WorkTime, BreakTime]),
	{ok,{interval,_Timer}} = timer:send_interval(1000,tick),
	Start = calendar:universal_time(),
	End   = calendar:gregorian_seconds_to_datetime(WorkTime+calendar:datetime_to_gregorian_seconds(Start)),
	State = #state{pomo_name=Name, workTime=WorkTime, breakTime=BreakTime, startTime=Start, endTime=End},
	{ok, work, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
work(Event, State) ->
	lager:warning("unexpected event @work: Event='~p', State='~p'", [Event, lager:pr(State,?MODULE)]),
	{next_state, work, State}.
break(Event, State) ->
	lager:warning("unexpected event @break: Event='~p', State='~p'", [Event, lager:pr(State,?MODULE)]),
	{next_state, break, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
work(Event, From, State) ->
	lager:warning("unexpected sync event @work: Event='~p', From='~p', State='~p'", [Event, From, lager:pr(State,?MODULE)]),
	Reply = unexpected,
	{reply, Reply, work, State}.
break(Event, From, State) ->
	lager:warning("unexpected sync event @break: Event='~p', From='~p', State='~p'", [Event, From, lager:pr(State,?MODULE)]),
	Reply = unexpected,
	{reply, Reply, break, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
	lager:warning("unexpected event: Event='~p', StateName='~p', State='~p'", [Event, StateName, lager:pr(State,?MODULE)]),
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
	lager:warning("unexpected sync event: Event='~p', From='~p', StateName='~p', State='~p'",
		[Event, From, StateName, lager:pr(State,?MODULE)]),
	Reply = ok,
	{reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(tick, work, State) ->
	{Days, {Hours, Minutes, Seconds}} = calendar:time_difference(calendar:universal_time(), State#state.endTime),
	Percent = calculate_percent(Days, {Hours, Minutes, Seconds}, State#state.workTime),
	io:format("~p WORK: ~p days and ~p hours, ~p minutes, ~p seconds left (~p% done)~n", [time(), Days, Hours, Minutes, Seconds, Percent]),
	if
		Days=<0, Hours=<0, Minutes=<0, Seconds=<0; Days<0 -> %work time has ended
			{Start, End} = getStartEnd(State#state.breakTime),
			NewState=State#state{startTime=Start, endTime=End},
			{next_state, break, NewState};
		true -> %work time hasn't ended
			{next_state, work, State}
	end;
handle_info(tick, break, State) ->
	{Days, {Hours, Minutes, Seconds}} = calendar:time_difference(calendar:universal_time(), State#state.endTime),
	Percent = calculate_percent(Days, {Hours, Minutes, Seconds}, State#state.breakTime),
	io:format("~p BREAK: ~p days and ~p hours, ~p minutes, ~p seconds left (~p% done)~n", [time(), Days, Hours, Minutes, Seconds, Percent]),
	if
		Days=<0, Hours=<0, Minutes=<0, Seconds=<0; Days<0 -> %break time has ended
			{Start, End} = getStartEnd(State#state.workTime),
			NewState=State#state{startTime=Start, endTime=End},
			{next_state, work, NewState};
		true -> %breakTime time hasn't ended
			{next_state, break, State}
	end;
handle_info(Info, StateName, State) ->
	lager:warning("unexpected info: Info='~p', StateName='~p', State='~p'", [Info, StateName, lager:pr(State,?MODULE)]),
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, StateName, State) ->
	lager:debug("terminate: Reason='~p', StateName='~p', State='~p'", [Reason, StateName, lager:pr(State,?MODULE)]),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, StateName, State, Extra) ->
	lager:notice("code change: OldVsn='~p', StateName='~p', State='~p', Extra='~p'",
		[OldVsn, StateName, lager:pr(State,?MODULE), Extra]),
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @doc return a new start and end timestamp
%% @end
-spec getStartEnd(calendar:second()) -> {calendar:datetime(), calendar:datetime()}.
getStartEnd(Duration) ->
	Start = calendar:universal_time(),
	End   = calendar:gregorian_seconds_to_datetime(Duration+calendar:datetime_to_gregorian_seconds(Start)),
	{Start, End}.
	

%% @doc calculate the elapsed time in percent
%% @end
-spec calculate_percent(non_neg_integer(), calendar:time(), calendar:second()) -> float().
calculate_percent(Days, Time, Duration) ->
	1-((Days*86400+calendar:time_to_seconds(Time)) / Duration). %a day has 86400 seconds

