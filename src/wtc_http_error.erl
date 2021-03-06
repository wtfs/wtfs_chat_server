%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2014 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc_http_error).

-export([respond/4]).

-define(UPDATE(Key,Value,In), lists:keyreplace(Key, 1, In, {Key, Value})).

respond(Code, Headers, [], Req) ->
	respond(Code, Headers, <<>>, Req);

respond(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = <<"404 Not Found: \"", Path/binary, "\"\n">>,
	Headers2 = ?UPDATE(<<"content-length">>,
			   integer_to_list(byte_size(Body)), Headers),
	Headers3 = ?UPDATE(<<"content-type">>, "text/plain", Headers2),
	{ok, Req3} = cowboy_req:reply(404, Headers3, Body, Req2),
	Req3;

respond(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = [ "HTTP Error: ",
		 integer_to_list(Code),
		 "(", httpStatusCodes:getDescription(Code), ")",
		 $\n ],
	Headers2 = ?UPDATE(<<"content-length">>,
			   integer_to_list(iolist_size(Body)), Headers),
	Headers3 = ?UPDATE(<<"content-type">>, "text/plain", Headers2),
	{ok, Req2} = cowboy_req:reply(Code, Headers3, Body, Req),
	Req2;

respond(_Code, _Headers, _Body, Req) ->
	io:format("code: ~p~nheaders: ~n~p~n~nbody: ~n~p~n~nreq: ~n~p~n~n",
		  [_Code, _Headers, _Body, Req]),
	Req.
