-module(test_suite).

%% API
-export([start/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start() ->
	application:start(ts).

stop() ->
	gen_server:call(?SERVER, {stop}).

start_test(Args) when is_list(Args) ->
	ts_server:start_test(Args).

start_test(ClientNum, Args) when is_integer(ClientNum), is_list(Args) ->
	ts_server:start_test(ClientNum, Args).

stop_test() ->
	ts_server:stop_test().

get_test_result() ->
	ts_server:get_test_result().

flush_test_result() ->
	ts_server:flush_test_result()

%%%===================================================================
%%% Internal functions
%%%===================================================================
