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
	RIn.


%% @doc returns user's home directory
%% @end
-spec get_user_home() -> string().
get_user_home() ->
	{ok, [[Home]]} = init:get_argument(home),
	Home.

-define(APPLICATION, wtc).
-define(BASEDIR, get_base_dir()).
-define(USER_HOME, get_user_home()).
-define(PRIVDIR, case code:priv_dir(?APPLICATION) of {error, _} -> ?BASEDIR++"/priv"; PrivDir -> PrivDir end).
-define(CONFIG_FILES, [
	?USER_HOME ++ "/.wtfs_chat.conf",
	"/etc/wtfs_chat.conf",
	?PRIVDIR ++ "/defaults.conf"
	]).

% constants
-define(CONSTANTS, [{"APPLICATION",atom_to_list(?APPLICATION)},{"BASEDIR",?BASEDIR},{"HOME",?USER_HOME}]).

