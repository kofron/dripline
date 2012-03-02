%% @doc dripline_conf_mon is one of the core dripline 'monitors'.  it 
%%		watches the dripline_conf database for changes to the dripline
%%		system configuration, and dispatches those events when it 
%%		receives them.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
%% @todo this module isn't actually connected to anybody - it does not
%%		actually dispatch events.
%% @todo this should also probably be turned into a behavior rather than
%%		trying to keep all of the monitors separate.  they perform very
%% 		similar functions.
-module(dripline_conf_mon).
-behavior(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
-export([waiting/2,waiting/3,connecting/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm callbacks and api %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
				{
					db_handle, 
					change_proc :: pid(),
					lastSeqNo :: integer()
				}).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc the connecting state is the very beginning of the state diagram.
%%		we are in it at two points during normal behavior - when we are
%%		first starting and when a socket gets closed and we need to 
%%		reconnect to the database.
%% @end
%%---------------------------------------------------------------------%%
connecting(timeout, #state{db_handle=Db}=StateData) ->
	case couchbeam_changes:stream(Db,self(),[continuous]) of
		{ok, _StartRef, ChPid} -> 
			{next_state, waiting, StateData#state{change_proc = ChPid}};
		{error, _Error} ->
			{next_state, connecting, StateData, 500}
	end.
%%---------------------------------------------------------------------%%
%% @doc waiting is the state that this gen_fsm is usually in.  basically 
%%		when the fsm is waiting for changes to the database, it is in 
%%		this state.
%% @end
%%---------------------------------------------------------------------%%
waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.
waiting(_Event, _From, StateData) ->
	{reply, ok, waiting, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	S = dripline_conn_mgr:get(),
	{ok, Db} = couchbeam:open_db(S,"dripline_conf"),
	{ok, connecting, #state{db_handle = Db, lastSeqNo = 0}, 1}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

handle_info({change, _Ref, {done, LastSeq}}, waiting, StateData) ->
	{next_state, connecting, StateData#state{lastSeqNo = LastSeq},1};
handle_info({change, _Ref, {_ChangeData}}, waiting, StateData) ->
	{next_state, waiting, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.