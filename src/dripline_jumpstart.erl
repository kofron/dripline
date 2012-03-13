%% @doc dripline_jumpstart is a process that gets the dripline ball
%%		rolling.  it is responsible for pulling all of the starting
%%		data from the database and informing dripling_conf_mgr.
%%		it exports a single function, go/0, and returns 'ok' on 
%%		successful startup.  Only in the case of a configuration 
%%		failure that means dripline cannot run at all should it return
%%		an error.
-module(dripline_jumpstart).
-export([go/0]).

-spec go() -> ok | {error, term()}.
go() ->
	ok.