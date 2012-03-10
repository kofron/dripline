%% @doc The dripline_dispatch module exports a single function, 
%%		dispatch/1, whose job it is to take documents from the
%%		couchdb changes feed (as processed by the cmd and conf
%%		monitors) and dispatch the processed result to the 
%%		appropriate actors.  The result is then sent back to the
%%		database.
%% @version 0.1a
%% @author Jared Kofron <jared.kofron@gmail.com>
-module(dripline_dispatch).
-behavior(gen_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% Exported API %%%
%%%%%%%%%%%%%%%%%%%%
-export([dispatch/1]).

%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API %%%
%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
-export([waiting/2,shipping/2,finishing/2]).
-export([resolving_type/2,resolving_channel/2,resolving_action/2]).
-export([report_error/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state records %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{cur_doc,cur_ch,cur_err,cur_fun}).

%%%%%%%%%%%%%%
%%% Macros %%%
%%%%%%%%%%%%%%
-define(NOW,0).
-define(DB,"dripline_cmd").

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported API Definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc dispatch/1 accepts a change document, strips off the document
%%		itself, and essentially parses down the command into funs.  the
%%		funs are then called by processes that are spawned from within
%%		the dispatcher.
%% @end
%%---------------------------------------------------------------------%%
-spec dispatch(ejson:json_object()) -> ok.
dispatch(DocUpdateLine) ->
	StrippedDoc = couchbeam_doc:get_value(<<"doc">>,DocUpdateLine),
	gen_fsm:send_all_state_event(?MODULE,{process,StrippedDoc}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm state definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(timeout,StateData) ->
	{next_state, waiting, StateData}.

resolving_type(timeout,#state{cur_doc=D}=StateData) ->
	Branch = case couchbeam_doc:get_value(<<"type">>,D) of
			<<"command">> ->
				{next_state,resolving_channel,StateData,?NOW};
			<<"system">> ->
				{next_state,finishing,StateData,?NOW};
			_Otherwise ->
				Err = {[{error,bad_command}]},
				NewStateData = StateData#state{
					cur_err = Err
				},
				{next_state,report_error,NewStateData,?NOW}
		end,
	Branch.

resolving_channel(timeout,#state{cur_doc=D}=StateData) ->
	CmdDoc = couchbeam_doc:get_value(<<"command">>,D),
	ChName = couchbeam_doc:get_value(<<"channel">>,CmdDoc),
	Branch = case dripline_conf_mgr:lookup(ChName) of
		{ok, Data} ->
			NewStateData = StateData#state{
				cur_ch = Data
			},
			{next_state,resolving_action,NewStateData,?NOW};
		{error, _E} ->
			Error = {[{error,unknown_channel}]},
			NewStateData = StateData#state{
				cur_err = Error
			},
			{next_state,report_error,NewStateData,?NOW}
		end,
	Branch.

resolving_action(timeout,#state{cur_doc=D,cur_ch=C}=StateData) ->
	CmdDoc = couchbeam_doc:get_value(<<"command">>,D),
	Branch = case couchbeam_doc:get_value(<<"do">>,CmdDoc) of
		<<"set">> ->
			Payload = couchbeam_doc:get_value(<<"value">>,CmdDoc),
			F = dripline_ch_data:synthesize_fun(C,Payload),
			NewStateData = StateData#state{
				cur_fun = F
			},
			{next_state, shipping, NewStateData, ?NOW};
		<<"get">> ->
			F = dripline_ch_data:synthesize_fun(C),
			NewStateData = StateData#state{
				cur_fun = F
			},
			{next_state, shipping, NewStateData, ?NOW};
		_Otherwise ->
			Error = {[{error,unknown_do_action}]},
			NewStateData = StateData#state{
				cur_err = Error
			},
			{next_state, report_error, NewStateData, ?NOW}
		end,
	Branch.

shipping(timeout,#state{cur_fun=F}=StateData) ->
	io:format("~p~n",[F]),
	{next_state,finishing,StateData,?NOW}.

report_error(timeout,#state{cur_doc=D,cur_err=E}=StateData) ->
	DocID = couchbeam_doc:get_id(D),
	{ok, _} = dripline_util:update_couch_doc(?DB,DocID,"result",E),
	{next_state,finishing,StateData,?NOW}.

finishing(timeout,_StateData) ->
	IdleState = idle_state(),
	{next_state,waiting,IdleState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm api definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	IdleState = idle_state(),
	{ok, waiting, IdleState, 0}.

handle_event({process, Doc}, waiting, StateData) ->
	NewStateData = StateData#state{cur_doc = Doc},
	{next_state, resolving_type, NewStateData, ?NOW}.

handle_sync_event(_Ev, _F, waiting, SData) ->
	{reply, ok, waiting, SData}.

handle_info(_Info, waiting, StateData) ->
	{next_state, waiting, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal function definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc idle_state/0 is just the default state for the fsm when it has 
%%		no data to process.
%% @end
%%---------------------------------------------------------------------%%
idle_state() ->
	#state{
		cur_doc = none,
		cur_ch = none,
		cur_err = none,
		cur_fun = none
	}.