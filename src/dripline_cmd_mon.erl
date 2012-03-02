%% @doc dripline_cmd_mon is one of the dripline 'monitor' modules.
%%		it watches the dripline_cmd database on a couchdb server for
%%		changes, and is responsible for dispatching those changes 
%%		asynchronously to a dispatcher.  it exports a notify/2 
%%		function which allows it to expect a document change to have
%%		a specific revision, which it will ignore.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version	0.1a
%% @todo might want to move some functions to a utility module, as
%%		they are called elsewhere.
-module(dripline_cmd_mon).
-behavior(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm internal states %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([waiting/2,waiting/3,connecting/2]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([notify/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callbacks %%%
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
					lastSeqNo :: integer(),
					revs
				}).

%%%%%%%%%%%%%%%%%%%%%
%%% API Functions %%%
%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc notify/2 mutates the state of the FSM so that it expects a 
%%		particular document change to carry a specific revision number.
%%		if the FSM receives a change that it has been previously notified
%%		of, it ignores it and does not pass it to the dispatching queue.
%%		this is, among other things, to avoid infinite loops due to 
%%		updating documents with message replies.
%% @end
%%---------------------------------------------------------------------%%
-spec notify(binary(),integer()) -> ok.
notify(DocId,RevNo) ->
	gen_fsm:sync_send_all_state_event(?MODULE,{notify,DocId,RevNo}).
	
%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc connecting is the state where the fsm begins its work by 
%%		telling couchdb it wants streaming changes from the dripline_cmd
%%		database.
%% @end
%%---------------------------------------------------------------------%%
connecting(timeout, #state{db_handle=Db,lastSeqNo=N}=StateData) ->
	StreamOpts = [continuous,include_docs,{since,N}],
	case couchbeam_changes:stream(Db,self(),StreamOpts) of
		{ok, _StartRef, ChPid} -> 
			{next_state, waiting, StateData#state{change_proc = ChPid}};
		{error, _Error} ->
			{next_state, connecting, StateData, 500}
	end.

%%---------------------------------------------------------------------%%
%% @doc waiting is the usual state of the fsm, where it waits for changes
%%		from couchdb.
%% @end
%%---------------------------------------------------------------------%%
waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.
waiting(_Event, _From, StateData) ->
	{reply, ok, waiting, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callbacks definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%% @todo we should clear the ignore flag after receiving a given update
%%		and maybe even log a warning if we don't get it after a certain
%%		amount of time.
handle_sync_event({notify,Id,RNo}, _F, SName, #state{revs=R}=SData) ->
	NewRevs = dict:update(Id, fun(_) -> RNo end,R),
	NewStateData = SData#state{revs=NewRevs},
	{reply, ok, SName, NewStateData}.

handle_info({change, _R, {done, LastSeq}}, waiting, StateData) ->
	{next_state, connecting, StateData#state{lastSeqNo = LastSeq},1};
handle_info({change, _R, {ChangeData}}, waiting, #state{revs=R}=SData) ->
	NewStateData = case ignore_update(ChangeData,R) of
		true ->
			SData;
		false ->
			NewRevs = update_rev_data(ChangeData,R),
			dripline_dispatch:dispatch(ChangeData),		
			SData#state{revs=NewRevs}
	end,
	{next_state, waiting, NewStateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc ignore_update/2 decides if an event should be ignored or not 
%%		based on existing data about that event.  if the fsm has 
%%		previously been notified about this changeset, it ignores it.
%%		otherwise, it does not.
%% @end
%%---------------------------------------------------------------------%%
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

%%---------------------------------------------------------------------%%
%% @doc update_rev_data adds an ignore flag to the revision dictionary 
%%		for a given document and revision tag.
%% @end
%%---------------------------------------------------------------------%%
update_rev_data(ChangeData,RevisionInfo) ->
	Doc = proplists:get_value(<<"doc">>,ChangeData),
	Id = couchbeam_doc:get_value(<<"_id">>,Doc),
	BinRev = couchbeam_doc:get_value(<<"_rev">>,Doc),
	Rev = strip_rev_no(BinRev),
	dict:store(Id,Rev,RevisionInfo).

%%---------------------------------------------------------------------%%
%% @doc strip_rev_no/1 takes a binary "_rev" tag and strips the revision
%% 		sequence number.  this is very useful for notifying the monitor
%%		that a sequence number is about to be changed by e.g. us.
%% @end
%%---------------------------------------------------------------------%%
-spec strip_rev_no(binary()) -> integer().
strip_rev_no(BinRev) ->
	[NS,_] = string:tokens(binary_to_list(BinRev),"-"),
	{N,[]} = string:to_integer(NS),
	N.

%%---------------------------------------------------------------------%%
%% @doc get_last_seq/1 gets the last update sequence for a given 
%%		database.  this provides the starting point for our changes 
%%		monitor.
%% @todo move this to a utility module.
%% @end
%%---------------------------------------------------------------------%%
-spec get_last_seq(couchbeam:db()) -> integer().
get_last_seq(DbHandle) ->
	{ok,{Info}} = couchbeam:db_info(DbHandle),
	proplists:get_value(<<"update_seq">>,Info).
