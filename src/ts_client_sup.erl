-module(ts_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	start_client/2,
        stop_all_clients/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
	inets:start(),
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(1, Args) when is_list(Args) ->
	supervisor:start_child(?SERVER, Args),
	ok;
start_client(ClientNum, Args) when is_integer(ClientNum), is_list(Args) ->
	supervisor:start_child(?SERVER, Args),
	start_client(ClientNum - 1, Args).

stop_all_clients() ->
	stop_all_clients(supervisor:which_chlidren(?SERVER)).

stop_all_clients([]) ->
	ok;
stop_all_clients([{_Id, Pid, _Type, _Module}|T]) ->
	supervisor:terminate_child(?SERVER, Pid),
	stop_all_clients(T).

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
