-module(dripline_cmd_mon).
-behavior(gen_fsm).

%% internal state data record
-record(state,
				{
					db_handle, 
					change_proc :: pid(),
					lastSeqNo :: integer(),
					revs
				}).

%% internal states
-export([waiting/2,waiting/3,connecting/2]).

%% API
-export([start_link/0]).
-export([notify/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%%%%%%%%%%%
%%% API Functions %%%
%%%%%%%%%%%%%%%%%%%%%
notify(DocId,RevNo) ->
	gen_fsm:sync_send_all_state_event(?MODULE,{notify,DocId,RevNo}).
	
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
		lastSeqNo = LastSeqNo,
		revs = dict:new()
	},
	{ok, connecting, InitialState, 0}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event({notify,DocId,RevNo}, _From, StateName, #state{revs=R}=StateData) ->
	NewRevs = dict:update(DocId, fun(_) -> RevNo end,R),
	NewStateData = StateData#state{revs=NewRevs},
	{reply, ok, StateName, NewStateData}.

handle_info({change, _Ref, {done, LastSeq}}, waiting, StateData) ->
	{next_state, connecting, StateData#state{lastSeqNo = LastSeq},1};
handle_info({change, _Ref, {ChangeData}}, waiting, #state{revs=R}=StateData) ->
	NewStateData = case ignore_update(ChangeData,R) of
		true ->
			StateData;
		false ->
			NewRevs = update_rev_data(ChangeData,R),
			dripline_dispatch:dispatch(ChangeData),		
			StateData#state{revs=NewRevs}
	end,
	{next_state, waiting, NewStateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
ignore_update(ChangeData,RevisionInfo) ->
	Doc = proplists:get_value(<<"doc">>,ChangeData),
	Id = couchbeam_doc:get_value(<<"_id">>,Doc),
	BinRev = couchbeam_doc:get_value(<<"_rev">>,Doc),
	Rev = strip_rev_no(BinRev),
	case dict:find(Id,RevisionInfo) of
		{ok, Rev} ->
			true;
		_ ->
			false
	end.

update_rev_data(ChangeData,RevisionInfo) ->
	Doc = proplists:get_value(<<"doc">>,ChangeData),
	Id = couchbeam_doc:get_value(<<"_id">>,Doc),
	BinRev = couchbeam_doc:get_value(<<"_rev">>,Doc),
	Rev = strip_rev_no(BinRev),
	dict:store(Id,Rev,RevisionInfo).

strip_rev_no(BinRev) ->
	[NS,_] = string:tokens(binary_to_list(BinRev),"-"),
	{N,[]} = string:to_integer(NS),
	N.

get_last_seq(DbHandle) ->
	{ok,{Info}} = couchbeam:db_info(DbHandle),
	proplists:get_value(<<"update_seq">>,Info).
