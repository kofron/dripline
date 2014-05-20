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
%
% It seems to have been decided that it isn't, though I'm not inclined to
% agree. Without acknowledgement, there is no garuntee of a response.
% Without indicating if there will be processed or not, it isn't clear if
% lack of further response is because the node doesn't care, or because the
% node is working. A nice behavior would be that for every command posted,
% every running node responds to indicate on of four states:
% 1) received, doc seems ill formed
% 2) received, doc not relevant to this node
% 3) received, this node is processing this doc -> expect further response
% 4) complete, previously in state (3) this node has finished responding
handle_cast({process, Command}, State) ->
    lager:info("command doc received"),
    case dl_compiler:compile(Command) of
    {ok, Request} ->
        lager:info("request received: ~p", [Request]),
        do_request_if_exists(Request, State);
    {error, Reason, BadRequest} ->
        Err = dl_compiler:compiler_error_msg(Reason),
        do_error_response(BadRequest, Err, State)
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _StateData) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_request_if_exists(RequestData, StateData) ->
    lager:notice("RequestData is: ~p",[dl_request:get_target(RequestData)]),
    lager:notice("is real: ~p", [dl_conf_mgr:is_real_channel(higgsino_disk)]),
    case dl_conf_mgr:is_real_channel(dl_request:get_target(RequestData)) of
    true ->
        do_request_if_local(RequestData, StateData);
    false ->
        lager:warning("channel unknown"),
        do_error_response(RequestData, "unrecognized_channel", StateData),
        ok
    end.

do_request_if_local(RequestData, StateData) ->
    case dl_conf_mgr:is_local_channel(dl_request:get_target(RequestData)) of
    true ->
        do_request(RequestData, StateData);
    false ->
        ok
    end.

do_request(RequestData, StateData) ->
    case dl_conf_mgr:mfa_from_request(RequestData) of
    {error, no_channel} ->
        ErrorMsg = "no such channel", 
        do_error_response(RequestData, ErrorMsg, StateData);
    MFA ->
        do_collect_data(MFA, RequestData, StateData)
    end,
    StateData.

do_error_response(RequestData, ErrorMsg, #state{cdb_handle=H}=StateData) ->
    NewJS = dl_util:new_json_obj(),
    NodeName = dl_util:node_name(),
    dbg:p(self(), m),
    Err = if is_list(ErrorMsg) -> erlang:iolist_to_binary(ErrorMsg);
       is_binary(ErrorMsg) -> ErrorMsg;
       true -> erlang:iolist_to_binary(io_lib:format("~p", [ErrorMsg]))
    end,
    Res = ej:set_p({erlang:atom_to_binary(NodeName, utf8), <<"error">>}, 
           NewJS, 
           Err),
    ok = update_cmd_doc(dl_request:get_id(RequestData), H, Res),
    StateData.

do_collect_data({M,F,A}, RequestData, #state{cdb_handle=H}=StateData) ->
    Dt = erlang:apply(M,F,A),
    Final = try_finalize_data(Dt, RequestData),
    case dl_data:get_code(Final) of
    ok ->
        NodeName = erlang:atom_to_binary(dl_util:node_name(), utf8),
        NewJS = dl_util:new_json_obj(),
        ResD = ej:set_p({NodeName, <<"result">>}, 
               NewJS, 
               dl_data:get_data(Final)),
        ResT = ej:set_p({NodeName, <<"timestamp">>},
                ResD,
                dl_util:make_ts()),
        ResF = ej:set_p({NodeName, <<"final">>},
                ResT,
                dl_data:get_final(Final)),
        
        update_cmd_doc(dl_request:get_id(RequestData), H, ResF);
    error ->
        lager:debug("Data Collection Error"),
        do_error_response(RequestData, 
                  dl_data:get_data(Final),
                 StateData)
    end.

try_finalize_data(Data, Request) ->
    ChName = dl_request:get_target(Request),
    try
    dl_hooks:apply_hooks(ChName,Data)
    catch
    C:E ->
        lager:notice("failed to apply hooks for channel ~p (~p:~p)",
               [ChName,C,E]),
        Data
    end.


update_cmd_doc(DocID, DBHandle, JSON) ->
    {ok, Doc} = couchbeam:open_doc(DBHandle, DocID),
    NewDoc = ej:set({<<"result">>}, Doc, JSON),
    % TODO: this is probably wrong.
    case couchbeam:save_doc(DBHandle, NewDoc) of
    {ok, _} ->
        ok;
    {error, conflict} ->
        update_cmd_doc_loop(DBHandle, NewDoc);
    {error, _Other}=Err ->
        lager:error("Doc update error"),
        Err
    end.

update_cmd_doc_loop(_DBHandle, _NewDoc) ->
    lager:info("looping to resolve conflict."),
    lager:error("there's no way this is currently correct"),
    ok.
