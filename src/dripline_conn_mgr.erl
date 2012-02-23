-module(dripline_conn_mgr).
-behavior(gen_server).

% internal server state
-record(state,{conn_gen}).

% API
-export([get/0]).

% starting and linking
-export([start_link/0]).

% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
		terminate/2,code_change/3]).

% API definitions
get() ->
	gen_server:call(?MODULE,new_conn).

handle_call(new_conn, _From, #state{conn_gen = F} = StateData) ->
	{reply, F(), StateData}.

handle_cast(_Req, StateData) ->
	{noreply, StateData}.

handle_info(_Info, StateData) ->
	{noreply, StateData}.

start_link() ->
	start_link("p8portal.phys.washington.edu",5984).

start_link(DBHost,DBPort) ->
	gen_server:start_link({local,?MODULE},?MODULE,[DBHost,DBPort],[]).

init([DBHost,DBPort]) ->
	% try to connect to the server.  if we can't, this whole endeavor is
	% a wash.  otherwise, we're good - start normally.
	Svr = couchbeam:server_connection(DBHost,DBPort),
	case couchbeam:server_info(Svr) of
		{ok, _} ->
			F = fun() -> couchbeam:server_connection(DBHost,DBPort) end,
			{ok, #state{conn_gen = F}};
		Else ->
			Else
	end.

terminate(_Rsn, _StateData) ->
	ok.

code_change(_OldVsn, StateData, _Extras) ->
	{ok, StateData}.