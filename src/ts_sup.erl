-module(ts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
	ClientSup = {ts_client_sup, {ts_client_sup, start_link, []},
		permanent, 5000, supervisor, [ts_client_sup]},
	Server = {ts_server, {ts_server, start_link, []}, permanent,
		5000, worker, [ts_server]},
        RestartStrategy = {one_for_one, 5, 10},
        {ok, {RestartStrategy, [ClientSup, Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
