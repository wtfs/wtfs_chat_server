%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wtc_http_error).

-export([respond/4]).

respond(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = <<"404 Not Found: \"", Path/binary, "\"\n">>,
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
			{<<"content-length">>, integer_to_list(byte_size(Body))}),
	Headers3 = lists:keyreplace(<<"content-type">>, 1, Headers2,
			{<<"content-type">>, "text/plain"}),
	{ok, Req3} = cowboy_req:reply(404, Headers3, Body, Req2),
	Req3;

respond(Code, Headers, [], Req) when is_integer(Code), Code >= 400 ->
	respond(Code, Headers, <<>>, Req);

respond(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = [
			"HTTP Error: ",
			integer_to_list(Code),
			"(", httpStatusCodes:getDescription(Code), ")",
			$\n
			],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
			{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	Headers3 = lists:keyreplace(<<"content-type">>, 1, Headers2,
			{<<"content-type">>, "text/plain"}),
	{ok, Req2} = cowboy_req:reply(Code, Headers3, Body, Req),
	Req2;

respond(_Code, _Headers, _Body, Req) ->
	Req.
