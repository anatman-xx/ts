ts
==

Test Suite

What's it?
	Ts is a Test Suite writen in Erlang.

What's includs?

How to use it?
	'test_suite:start()'
		Start the application

	'test_suite:start_test(ClientNum, Args)'
		Start test with specify arguments
			-'ClientNum'	number of process
			-'Args'		arguments of client

	'test_suite:get_test_result()'
		Fethc test report
