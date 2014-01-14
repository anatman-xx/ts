-module(ts_client).

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
         running_test/2,
	 finished/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {url, count, intval}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Url, Count, Intval) when is_integer(Count), is_integer(Intval) ->
        gen_fsm:start_link(?MODULE, [Url, Count, Intval], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Url, Count, Intval]) ->
	inets:start(),
        {ok, running_test, #state{url = Url, count = Count, intval = Intval}, 0}.

running_test(timeout, State) when State#state.count > 0 ->
	case httpc:request(State#state.url) of
		{ok, {_Status, _Header, _Data}} ->
			ok;
		_ ->
			io:format("error~n"),
			ok
	end,
        {next_state, running_test, State#state{count = State#state.count - 1}, State#state.intval};

running_test(timeout, State) when State#state.count =< 0 ->
	{next_state, finished, State}.

finished(_Event, State) ->
	{next_state, finished, State}.

handle_event(_Event, StateName, State) ->
        {next_state, ok, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
        Reply = ok,
        {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
        {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
        ok.

code_change(_OldVsn, StateName, State, _Extra) ->
        {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


