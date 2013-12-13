%% dl_cdb_adapter.erl
%% @doc This is the dripline couchdb adapter.  It is responsible
%%      for relaying messages from couchdb to dripline, and also
%%      for pushing updates from dripline back to couchdb.  It listens
%%      on both the couchdb changes feed *and* on the dripline softbus.
%%      When data is presented on the softbus that it believes should
%%      be logged to couchdb, it will automagically push it upstream.
%%      When configuration changes come down the couchdb changes feed,
%%      it will push them onto the softbus.  Commands are handled by
%%      spawning a process which will handle the command and subsequently
%%      push the result back to couch.
%%
%%      Couchbeam is intentionally 'firewalled' in this module to avoid
%%      couchbeam dependencies being splattered all over dripline.  
%%      The hope is that this will, in the future, provide for a more
%%      generic adapter structure.
-module(dl_cdb_adapter).
-behavior(gen_dl_agent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State records and such %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(cdb_loc, {
	  host, 
	  port
	 }).
-record(state, {
	  revs,
	  db_cnf_hndl,
	  db_cmd_hndl,
	  conf_ch_ref,
	  cmd_ch_ref,
	  db_loc = #cdb_loc{}
	 }).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
%-export([notify/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_dl_agent callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/2,
	 init/1,
	 handle_sb_msg/2,
	 handle_info/2,
	 handle_cast/2,
	 handle_call/3,
	 code_change/3,
	 terminate/2]).

%%%%%%%%%%%%%%%%%%%%%
%%% Callback defs %%%
%%%%%%%%%%%%%%%%%%%%%
start_link(?MODULE, _Args) ->
    gen_dl_agent:start_link(?MODULE, ?MODULE).

init(_Args) ->
    {ok, {Host,Port}} = application:get_env(dl_core, couch_host),
    DbConn = couchbeam:server_connection(Host,Port),
    {ok, ConfDbHndl} = couchbeam:open_db(DbConn, "dripline_conf"),
    {ok, CmdDbHndl} = couchbeam:open_db(DbConn, "dripline_cmd"),
    {ok, ConfRef} = setup_conf_streaming(ConfDbHndl),
    {ok, CmdRef} = setup_cmd_streaming(CmdDbHndl),
    InitialState = #state{
      revs = dict:new(),
      db_cnf_hndl = ConfDbHndl,
      db_cmd_hndl = CmdDbHndl,
      conf_ch_ref = ConfRef,
      cmd_ch_ref = CmdRef,
      db_loc = #cdb_loc{
	host = Host,
	port = Port
       }
     },
    {ok, InitialState}.

%% Because this process can push messages into the softbus, we ignore
%% messages that we actually sent.
handle_sb_msg({_Ref, dl_cdb_adapter, _Msg}, #state{}=State) ->
    {noreply, State};
%% All other softbus messages are handled here.
handle_sb_msg({_Ref, _AnyID, {nd, {Instr, Chan}, Data}}, #state{}=SD) ->
    spawn(fun() -> worker_dt(Instr,Chan,Data) end),
    {noreply, SD};
handle_sb_msg({_Ref, _AnyID, _Msg}, #state{}=State) ->
    {noreply, State}.

%% When our streams go down, recuisitate them
handle_info({change, R, {done, _LastSeq}}, 
	    #state{cmd_ch_ref=R,db_cmd_hndl=H}=State) ->
    {ok, CmdRef} = setup_cmd_streaming(H),
    {noreply, State#state{cmd_ch_ref=CmdRef}};
handle_info({change, R, {done, _LastSeq}}, 
	    #state{conf_ch_ref=R,db_cnf_hndl=H}=State) ->
    {ok, ConfRef} = setup_conf_streaming(H),
    {noreply, State#state{conf_ch_ref=ConfRef}};
%% We get two kinds of changes.  The first kind comes from the 
%% configuration stream:
handle_info({change, R, ChangeData}, #state{conf_ch_ref=R, revs=_Revs}=State) ->
    dl_softbus:bcast(agents, ?MODULE, ChangeData),
    {noreply, State};
%% The second kind of changes come from the command stream.
handle_info({change, R, ChangeData}, #state{cmd_ch_ref=R, revs=Revs}=State) ->
    NewState = case ignore_update_rev(ChangeData, Revs) of
		   true ->
		       State;
		   false ->
		       spawn_cmd_worker(ChangeData),
		       State#state{revs=update_rev_data(ChangeData,Revs)}
	       end,    
    {noreply, NewState}.

handle_call(_Call, _From, StateData) ->
    {reply, ok, StateData}.

handle_cast(_Cast, StateData) ->
    {noreply, StateData}.

code_change(_Version, StateData, _Extra) ->
    {ok, StateData}.

terminate(_Reason, _StateData) ->
    ok.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------%%
%% @doc setup_conf_streaming sets us up so that configuration changes are
%%      sent over the changes API to us.  Whereas with the command stream
%%      we want all of the changes since the last seq, with configuration
%%      we actually want all of them ever.  This gives us, on startup, the
%%      'latest' configuration documents that are there - which 
%%      effectively sets up the system.
%%----------------------------------------------------------------------%%
-spec setup_conf_streaming(couchbeam:db()) -> ok | {error, term()}.
setup_conf_streaming(DbHandle) ->
    StreamOpts = [continuous, include_docs, {since, 0}],
    case couchbeam_changes:stream(DbHandle, self(), StreamOpts) of
	{ok, StartRef, _ChPid} ->
	    {ok, StartRef};
	{error, _Error}=E ->
	    E
    end.

%%----------------------------------------------------------------------%%
%% @doc setup_cmd_streaming sets up the adapter to receive commands via
%%      couchdb.  This is a little different from the configuration feed
%%      because we definitely do not want to process every command since
%%      the dawn of the database.  Instead, we only get changes to the DB
%%      since the last sequence number.
%%----------------------------------------------------------------------%%
-spec setup_cmd_streaming(couchbeam:db()) -> ok | {error, term()}.
setup_cmd_streaming(DbHandle) ->
    {ok,Info} = couchbeam:db_info(DbHandle),
    LastSeq = props:get('update_seq',Info),
    StreamOpts = [continuous, include_docs, {since, LastSeq}],
    case couchbeam_changes:stream(DbHandle, self(), StreamOpts) of
	{ok, StartRef, _ChPid} ->
	    {ok, StartRef};
	{error, _Error}=E ->
	    E
    end.

%%---------------------------------------------------------------------%%
%% @doc update_rev_data adds an ignore flag to the revision dictionary 
%%		for a given document and revision tag.
%% @end
%%---------------------------------------------------------------------%%
update_rev_data(ChangeData,RevisionInfo) ->
	Id = props:get('doc._id',ChangeData),
	BinRev = props:get('doc._rev',ChangeData),
	Rev = strip_rev_no(BinRev),
	dict:store(Id,Rev + 1,RevisionInfo).

%%----------------------------------------------------------------------%%
%% @doc When a document passes through the changes feed, we automatically
%%      ignore the next update.  This should work just fine.
%%----------------------------------------------------------------------%%
-spec ignore_update_rev(ejson:json_object(), dict()) -> boolean().
ignore_update_rev(ChangeLine, RevsDict) ->
    DocID = props:get('doc._id', ChangeLine),
    BinRev = props:get('doc._rev', ChangeLine),
    RevNo = strip_rev_no(BinRev),
    case dict:find(DocID, RevsDict) of
	{ok, RevNo} ->
	    true;
	_ ->
	    false
    end.

%%---------------------------------------------------------------------%%
%% @doc spawn_cmd_worker/1 fires off a process to deal with a new command.
%% this includes compiling, resolving the host, etc etc.  
%% @end
%%---------------------------------------------------------------------%%
-spec spawn_cmd_worker(ejson:json_object()) -> pid().
spawn_cmd_worker(Command) ->
    ok = poolboy:transaction(couchdb_command_pool,
			     fun(Worker) ->
				     gen_server:cast(Worker, {process, Command})
			     end).

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

%%----------------------------------------------------------------------%%
%% @doc This is a specialized worker for responding to data takers.  It
%%      takes data *already read* and pushes it up to the data log on 
%%      the couch database.
%%----------------------------------------------------------------------%%
-spec worker_dt(atom(),atom(),binary()) -> ok.
worker_dt(Instr,Ch,RawData) ->
    DlDt = dl_data:from_prologix(RawData),
    ChInfo = dl_conf_mgr:channel_info(Instr, Ch),
    ChName = dl_ch_data:get_id(ChInfo),
    HookedData = try
		     dl_hooks:apply_hooks(ChName,DlDt)
		 catch
		     C:E ->
			 lager:info("failed to apply hooks for channel ~p (~p:~p) [~p,~p]",
				    [ChName,C,E,DlDt,ChInfo]),
			 DlDt
		 end,
    CouchDoc = dl_dt_data_to_couch(ChName,HookedData),
    post_dt_couch_doc(CouchDoc).

%%----------------------------------------------------------------------%%
%% @doc Post a data point to couchdb by creating a new document.
%%----------------------------------------------------------------------%%
-spec post_dt_couch_doc(term()) -> ok.
post_dt_couch_doc(CD) ->
    NewDoc = couchbeam_doc:extend(CD,{[]}),
    {ok, {Host,Port}} = application:get_env(dl_core, couch_host),
    DbConn = couchbeam:server_connection(Host,Port),
    {ok, Db} = couchbeam:open_or_create_db(DbConn,"dripline_logged_data"),
    {ok, _Doc} = couchbeam:save_doc(Db,NewDoc).

%%----------------------------------------------------------------------%%
%% @doc dl_dt_data_to_couch just translates a dl_data structure into a 
%%      couch-friendly representation.  In this case, it is a proplist of
%%      binary pairs and has been collected by a data taker.
%%----------------------------------------------------------------------%%
-spec dl_dt_data_to_couch(atom(),dl_data:dl_data()) -> [{binary(),binary()}].
dl_dt_data_to_couch(Name,DlDt) ->
    SName = erlang:atom_to_binary(Name,latin1),
    case dl_data:get_code(DlDt) of
	ok ->
	    Rs = dl_data:get_data(DlDt),
	    Ts = dl_data:get_ts(DlDt),
	    Fn = dl_data:get_final(DlDt),
	    [
	     {<<"sensor_name">>, SName},
	     {<<"uncalibrated_value">>, Rs},
	     {<<"timestamp_localstring">>, Ts},
	     {<<"calibrated_value">>, Fn}
	    ]
    end.
	    
