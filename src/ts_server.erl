-module(ts_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         start_test/1,
         start_test/2,
         stop_test/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CLIENT_NUM, 10).
-define(MAX_CLIENT_NUM, 1024).

-record(state, {sup, result}).
-record(result, {success, timeout, error}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, {stop}).

start_test(Args) when is_list(Args) ->
	gen_server:call(?SERVER, {start_test, ?DEFAULT_CLIENT_NUM, Args}).

start_test(ClientNum, Args) when is_integer(ClientNum), is_list(Args) ->
	gen_server:call(?SERVER, {start_test, ClientNum, Args}).

stop_test() ->
	gen_server:call(?SERVER, {stop_test}).

report_test_result(TestResult) ->
	gen_server:cast(?SERVER, {report_test_result, TestResult}).

get_test_result() ->
	gen_server:call(?SERVER, {get_test_result}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
	{ok, SupRef} = ts_client_sup:start_link(),
	{ok, #state{sup = SupRef, result = #result{success = 0, timeout = 0, error = 0}}}.

handle_call({stop}, _From, State) ->
	{stop, normal, ok, State};
handle_call({start_test, ClientNum, Args}, _From, State) ->
	ok = ts_client_sup:start_client(ClientNum, Args),
        {reply, ok, State};
handle_call({stop_test}, _From, State) ->
        {reply, ok, State};
handle_call({get_test_result}, _From, State) ->
	{reply, State#state.result};
handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({report_test_result, {success, {TimeDeltaA, TimeDeltaB, TimeDeltaC}}}, State) ->
	if
		TimeDeltaB >= 200 ->
			{noreply, State#state{result = State#state.result#result{success = State#state.result#result.success + 1}}};
		TimeDeltaB < 200 ->
			{noreply, State#state{result = State#state.result#result{timeout = State#state.result#result.timeout + 1}}};
		true ->
			{noreply, State}
	end.

handle_cast({report_test_result, {error}}, State) ->
	{noreply, State#state{result = }};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




