-module(ts_client_sup).

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
        Child = {ts_client, {ts_client, start_link, []}, temporary, brutal_kill,
		worker, [ts_client]},
        RestartStrategy = {simple_one_for_one, 0, 1},

        {ok, {RestartStrategy, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
