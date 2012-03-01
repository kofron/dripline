-module(dripline_cmd_mon).
-behavior(gen_fsm).

%% internal state data record
-record(state,
				{
					db_handle, 
					change_proc :: pid(),
					lastSeqNo :: integer()
				}).

%% internal states
-export([waiting/2,waiting/3,connecting/2]).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

% states
connecting(timeout, #state{db_handle=Db,lastSeqNo=N}=StateData) ->
	case couchbeam_changes:stream(Db,self(),[continuous,include_docs,{since,N}]) of
		{ok, _StartRef, ChPid} -> 
			{next_state, waiting, StateData#state{change_proc = ChPid}};
		{error, _Error} ->
			{next_state, connecting, StateData, 500}
	end.

waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.
waiting(_Event, _From, StateData) ->
	{reply, ok, waiting, StateData}.

% gen_fsm callbacks
start_link() ->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	S = dripline_conn_mgr:get(),
	{ok, Db} = couchbeam:open_db(S,"dripline_cmd"),
	LastSeqNo = get_last_seq(Db),
	InitialState = #state{
		db_handle = Db,
		lastSeqNo = LastSeqNo
	},
	{ok, connecting, InitialState, 0}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

handle_info({change, _Ref, {done, LastSeq}}, waiting, StateData) ->
	{next_state, connecting, StateData#state{lastSeqNo = LastSeq},1};
handle_info({change, _Ref, {ChangeData}}, waiting, StateData) ->
	LastSeq = proplists:get_value(<<"seq">>,ChangeData),
	dripline_dispatch:dispatch(ChangeData),
	{next_state, waiting, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
get_last_seq(DbHandle) ->
	{ok,{Info}} = couchbeam:db_info(DbHandle),
	proplists:get_value(<<"update_seq">>,Info).