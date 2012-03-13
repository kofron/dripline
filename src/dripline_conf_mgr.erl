%% @doc dripline_conf_mgr is one of the core dripline 'managers' and is
%%		probably the most important module in the entire codebase.  it 
%%		stores all of the state regarding the current configuration, and
%%		is able to generate executable fun()s from that state.  whenever
%%		a query or command is sent through the database, calls are made
%%		to this module in order to determine what to do.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
%% @todo clean this up for god's sake
%% @todo memoizing state would increase efficiency
-module(dripline_conf_mgr).
-behavior(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([add_channel/1,lookup/1,all_channels/0]).
-export([get_logger_pid/1,set_logger_pid/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{
			chs, % Channel data
			lgs  % Logger data
		}).

%%%%%%%%%%%%%%%%%%%%%%
%%% API definition %%%
%%%%%%%%%%%%%%%%%%%%%%
-spec add_channel(term()) -> ok | {error,already_exists}.
add_channel(ChannelData) ->
	gen_server:call(?MODULE,{add_channel, ChannelData}).

-spec lookup(binary()) -> {ok,term()} | {error,term()}.
lookup(ChName) ->
	gen_server:call(?MODULE,{lookup,{ch,ChName}}).

-spec all_channels() -> list().
all_channels() ->
	gen_server:call(?MODULE,all_channels).

-spec get_logger_pid(binary()) -> {ok,pid()} | {error,no_logger}.
get_logger_pid(ChannelName) ->
	gen_server:call(?MODULE,{get_lg_pid, ChannelName}).

-spec set_logger_pid(binary(),pid()) -> ok.
set_logger_pid(ChannelName, Pid) ->
	gen_server:call(?MODULE,{set_lg_pid, ChannelName, Pid}).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	SConn = dripline_conn_mgr:get(),
	ok = dripline_jumpstart:go(),
	{ok, Db} = couchbeam:open_db(SConn,"dripline_conf"),
	{ok, AllInstr} = couchbeam_view:fetch(Db,{"objects","instruments"}),
	{ok, AllChannels} = couchbeam_view:fetch(Db,{"objects","channels"}),
	{ok, ChanDict} = generate_channel_dict(AllChannels,AllInstr),
	InitialState = #state{
		chs = ChanDict,
		lgs = dict:new()
	},
	{ok, InitialState}.

handle_call({add_channel,Data}, _F, #state{chs=Ch}=StateData) ->
	ChannelName = dripline_ch_data:get_fields(id,Data),
	{NewStateData, Reply} = case dict:is_key(ChannelName,Ch) of
		true ->
			{StateData, {error, already_exists}};
		false ->
			TNewStateData = StateData#state{
				chs = dict:store(ChannelName,Data)
			},
			{TNewStateData, ok}
	end,
	{reply, Reply, NewStateData};
handle_call({lookup,{ch,Name}}, _From, #state{chs=Ch}=StateData) ->
	Reply = case dict:find(Name,Ch) of
		{ok, Value} ->
			{ok, Value};
		error ->
			{error, {bad_channel,Name}}
	end,
	{reply, Reply, StateData};
handle_call(all_channels, _F, #state{chs=Ch}=StateData) ->
	Reply = dict:fetch_keys(Ch),
	{reply, Reply, StateData};
handle_call({get_lg_pid, Name}, _F, #state{lgs=Lg}=StateData) ->
	Reply = case dict:find(Name,Lg) of
		{ok, _Pid}=C ->
			C;
		error ->
			{error, no_logger}
	end,
	{reply, Reply, StateData};
handle_call({set_lg_pid, Name, Pid}, _F, #state{lgs=Lg}=StateData) ->
	erlang:monitor(process,Pid),
	NewState = StateData#state{
		lgs = dict:store(Name,Pid,Lg)
	},
	{reply, ok, NewState}.

handle_cast(_Cast, StateData) ->
	{noreply, StateData}.

handle_info({'DOWN',_Ref, process, Pid, Reason}, #state{lgs=Lg}=SData) ->
	NewStateData = case Reason of
		normal -> %% need to drop this pid from dictionary
			SData#state{
				lgs = drop_pid(Lg,Pid)
			};
		shutdown -> %% need to drop this pid from dictionary
			SData#state{
				lgs = drop_pid(Lg,Pid)
			};
		_Other -> %% Should we try to track down the new process?
			SData
	end,
	{noreply, NewStateData};
handle_info(_Info, StateData) ->
	{stop, unexpected_info, StateData}.

terminate(_Reason, _StateData) ->
	ok.

code_change(_OldVsn, StateData, _Extra) ->
	{ok, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_channel_dict(ChViewRes,InViewRes) ->
	[StrippedCh,StrippedIn] = lists:map(fun(X) -> 
											strip_values(X) 
										end, 
										[ChViewRes,InViewRes]),
	generate_channel_dict(StrippedCh,StrippedIn,dict:new()).
generate_channel_dict([],_,Acc) ->
	{ok,Acc};
generate_channel_dict([H|T],Instr,Acc) ->
	InstrId = couchbeam_doc:get_value(<<"instrument">>,H),
	case get_call_data(InstrId,Instr) of
		{ok, [Name,Model]} ->
			CD0 = dripline_ch_data:new(),
			CD1 = dripline_ch_data:set_field(instr,Name,CD0),
			CD2 = dripline_ch_data:set_field(model,Model,CD1),
			ChName = couchbeam_doc:get_value(<<"name">>,H),
			CD3 = dripline_ch_data:set_field(id,ChName,CD2),
			Locator = couchbeam_doc:get_value(<<"locator">>,H),
			CD4 = dripline_ch_data:set_field(locator,Locator,CD3),
			generate_channel_dict(T,Instr,dict:store(ChName,CD4,Acc));
		{error, _E}=Err ->
			Err
	end.

strip_values(L) ->
	lists:map(fun(X) -> couchbeam_doc:get_value(<<"value">>,X) end, L).
get_call_data(_,[]) ->
	{error,instrument_not_found};
get_call_data(Id,[In|Ins]) ->
	case couchbeam_doc:get_id(In) of
		Id ->
			Name = couchbeam_doc:get_value(<<"name">>,In),
			Model = couchbeam_doc:get_value(<<"instrument_model">>,In),
			{ok, [Name,Model]};
		_NotId ->
			get_call_data(Id,Ins)
	end.

drop_pid(LogDict,Pid) ->
	Search = fun(_K,_V,{true,_D2}=NoOp) -> 
							NoOp;
				 (K,Pid,{false,D1}) ->
				 			{true, dict:erase(K,D1)}
	end,
	{true, D2} = dict:fold(Search,{false,LogDict},LogDict),
	D2.
