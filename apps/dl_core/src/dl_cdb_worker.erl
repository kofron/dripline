%% poolboy_cdb_worker is a member of the worker pool that handles requests which come in over
%% the couchdb changes feed.
-module(dl_cdb_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2,
         code_change/3]).

-record(state, {cdb_conn, cdb_handle}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    % TODO: this should be centralized somewhere.  dl_cdb_adapter perhaps?
    % using configuration data is OK for now but why have a bunch of processes
    % trying to connect to couch?  gproc:await could be used to wait for 
    % dl_cdb_adapter, and the adapter could hand out connections.
    {ok, {Host, Port}} = application:get_env(dl_core, couch_host),
    DbConn = couchbeam:server_connection(Host, Port),
    {ok, CmdDbHandle} = couchbeam:open_db(DbConn, "dripline_cmd"),
    {ok, #state{cdb_conn = DbConn, cdb_handle = CmdDbHandle}}.

handle_call(_Call, _From, State) ->
    % should die here, no calls are expected
    {reply, ok, State}.
% when a command is sent to the worker, the following happens:
%   1) an attempt is made to compile the command to a dl_request
%   1a) if that succeeds, then we enter do_request
%   1b) if that fails, we enter do_error_response
% FUTURE: the document is already acknowledged at this point, so
% we can update with impunity.  first we should update to say 
% if we will process this request or not, then update with whatever
% the result may be.  is all this communication necessary?  maybe
handle_cast({process, Command}, State) ->
    case dl_compiler:compile(Command) of
	{ok, Request} ->
	    do_request(Request, State);
	{error, Reason, BadRequest} ->
	    do_error_response(BadRequest, Reason, State)
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _StateData) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_request(RequestData, #state{cdb_handle=H}=StateData) ->
    NewJS = dl_util:new_json_obj(),
    NodeName = dl_util:node_name(),
    Res = ej:set_p({erlang:atom_to_binary(NodeName, utf8), <<"ok">>}, 
		 NewJS, 
		 <<"data">>),
    ok = update_cmd_doc(dl_request:get_id(RequestData), H, Res),
    StateData.
do_error_response(RequestData, Error, #state{cdb_handle=H}=StateData) ->
    NewJS = dl_util:new_json_obj(),
    NodeName = dl_util:node_name(),
    Err = dl_compiler:compiler_error_msg(Error),
    Res = ej:set_p({erlang:atom_to_binary(NodeName, utf8), <<"error">>}, 
		   NewJS, 
		   erlang:iolist_to_binary(Err)),
    ok = update_cmd_doc(dl_request:get_id(RequestData), H, Res),
    StateData.

update_cmd_doc(DocID, DBHandle, JSON) ->
    {ok, Doc} = couchbeam:open_doc(DBHandle, DocID),
    NewDoc = ej:set({<<"result">>}, Doc, JSON),
    % TODO: this is probably wrong.
    StampedDoc = ej:set({<<"timestamp">>}, NewDoc, dl_util:make_ts()), 
    case couchbeam:save_doc(DBHandle, StampedDoc) of
	{ok, _} ->
	    ok;
	{error, conflict} ->
	    update_cmd_doc_loop(DBHandle, StampedDoc);
	{error, _Other}=Err ->
	    Err
    end.

update_cmd_doc_loop(_DBHandle, _NewDoc) ->
    io:format("looping to resolve conflict.~n"),
    ok.
