%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2014 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc_ws).

-behaviour(gen_event).
-behaviour(cowboy_websocket_handler).

%% API
-export([]).

%% cowboy_websocket_handler callbacks
-export([init/3,
	 websocket_init/3,
	 websocket_info/3,
	 websocket_handle/3,
	 websocket_terminate/3]).

%% gen_event callbacks
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {pid}).

%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% cowboy_websocket_handler callbacks
%%%===================================================================

%% @doc Upgrade the protocol to cowboy_websocket.
%% @end
init({_TransportName, _ProtocolName}, _Req, _Opts) ->
	lager:info("upgrade to websocket"),
	io:format("proto: ~p, trans: ~p, req: ~p", [_TransportName, _ProtocolName, _Req]),
	{upgrade, protocol, cowboy_websocket}.

%% @doc
%% Initialize the state for this session.
%%
%% This function is called before the upgrade to Websocket occurs. It can be
%% used to negotiate Websocket protocol extensions with the client. It will
%% typically be used to register this process to an event manager or a message
%% queue in order to receive the messages the handler wants to process.
%%
%% The connection will stay up for a duration of up to Timeout milliseconds
%% after it last received data from the socket, at which point it will stop
%% and close the connection. By default this value is set to infinity. It is
%% recommended to either set this value or ensure by any other mechanism that
%% the handler will be closed after a certain period of inactivity.
%%
%% The hibernate option will hibernate the process until it starts receiving
%% either data from the Websocket connection or Erlang messages.
%%
%% The shutdown return value can be used to close the connection before
%% upgrading to Websocket.
%% @end
websocket_init(_TransportName, Req, _Opts) ->
	wtc_lw_pomo:add_handler({?MODULE, self()}, #state{pid=self()}),
	State = #state{},
	{ok, Req, State}.

%% @doc 
%% Handle the Erlang message received.
%%
%% This function will be called every time an Erlang message has been
%% received. The message can be any Erlang term.
%%
%% The shutdown return value can be used to close the connection. A close
%% reply will also result in the connection being closed.
%%
%% The hibernate option will hibernate the process until it receives another
%% message or new data from the Websocket connection.
%% @end
websocket_info(Info, Req, State) ->
	lager:warning("unexpected info: Info='~p', State='~p'", [Info, lager:pr(State,?MODULE)]),
	{ok, Req, State}.

%% @doc
%% Handle the data received from the websocket connection.
%%
%% This function will be called every time data is received from the Websocket
%% connection.
%%
%% The shutdown return value can be used to close the connection. A close
%% reply will also result in the connection being closed.
%%
%% The hibernate option will hibernate the process until it receives new data
%% from the Websocket connection or an Erlang message.
%% @end
websocket_handle(InFrame, Req, State) ->
	lager:warning("unexpected handle: InFrame='~p', State='~p'", [InFrame, lager:pr(State,?MODULE)]),
	{ok, Req, State}.

%% @doc
%% Perform any necessary cleanup of the state.
%% 
%% The connection will be closed and the process stopped right after this
%% call.
%% @end
websocket_terminate(Reason, _Req, State) ->
	lager:debug("Reason: ~p, State: ~p", [Reason, lager:pr(State,?MODULE)]),
	ok.


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
init(State) ->
	lager:debug("State: ~p", [lager:pr(State,?MODULE)]),
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
handle_event({pomoState, _, _}, State) ->
	{ok, State};
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
handle_info({'EXIT', _, _}, State) ->
	{ok, State};
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
