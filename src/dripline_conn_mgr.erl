%% @doc dripline_conn_mgr is one of the dripline core 'managers'.  it
%% 		has a very simple task - hand out connections to the database
%%		when it is asked to.  for now it is incredibly simple and just
%%		provides a connection to anybody, anytime.  this may require some
%%		form of connection pooling in the future - this module is 
%%		designed to guarantee an API.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(dripline_conn_mgr).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% server state record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{conn_gen}).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([get/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
		terminate/2,code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec get() -> any().
get() ->
	gen_server:call(?MODULE,new_conn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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