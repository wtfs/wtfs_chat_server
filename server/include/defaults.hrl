%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% default values
%%% @end
%%%-------------------------------------------------------------------

%% @doc get base directory
%% @end
-spec get_base_dir() -> string().
get_base_dir() ->
	{ok, RIn} = file:get_cwd(),
	re:replace(RIn,"/server","",[{return,list}]).


%% @doc returns user's home directory
%% @end
-spec get_user_home() -> string().
get_user_home() ->
	{ok, [[Home]]} = init:get_argument(home),
	Home.

-define(APPLICATION, wtfs_chat).
-define(BASEDIR, get_base_dir()).
-define(USER_HOME, get_user_home()).
-define(CONFIG_FILES, [?USER_HOME ++ "/.wtfs_chat.conf", "/etc/wtfs_chat.conf", ?BASEDIR ++ "/defaults.conf"]).

% constants
-define(CONSTANTS, [{"APPLICATION",atom_to_list(?APPLICATION)},{"BASEDIR",?BASEDIR},{"HOME",?USER_HOME}]).

%-define(START_HTTP, true).
%-define(START_HTTPS, false).
%-define(HTTP_PORT, 60000).
%-define(HTTPS_PORT, 60001).
%-define(SSL_PEM_FILE, "").

