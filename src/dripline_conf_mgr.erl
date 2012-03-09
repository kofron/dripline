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
-export([lookup/1]).

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
			chs
		}).

%%%%%%%%%%%%%%%%%%%%%%
%%% API definition %%%
%%%%%%%%%%%%%%%%%%%%%%
-spec lookup(binary()) -> {ok,term()} | {error,term()}.
lookup(ChName) ->
	gen_server:call(?MODULE,{lookup,{ch,ChName}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	SConn = dripline_conn_mgr:get(),
	{ok, Db} = couchbeam:open_db(SConn,"dripline_conf"),
	{ok, AllInstr} = couchbeam_view:fetch(Db,{"objects","instruments"}),
	{ok, AllChannels} = couchbeam_view:fetch(Db,{"objects","channels"}),
	{ok, ChanDict} = generate_channel_dict(AllChannels,AllInstr),
	InitialState = #state{
		chs = ChanDict
	},
	{ok, InitialState}.

handle_call({lookup,{ch,Name}}, _From, #state{chs=Ch}=StateData) ->
	Reply = case dict:find(Name,Ch) of
		{ok, Value} ->
			{ok, Value};
		error ->
			{error, {bad_channel,Name}}
	end,
	{reply, Reply, StateData}.

handle_cast(_Cast, StateData) ->
	{noreply, StateData}.

handle_info(_Info, StateData) ->
	{noreply, StateData}.

terminate(_Reason, _StateData) ->
	ok.

code_change(_OldVsn, StateData, _Extra) ->
	{ok, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%---------------------------------------------------------------------%%
%% @doc normalize_instrument_name/2 is a horrible function that right now
%%		does entirely too much.  what it _should_ do is transform a 
%%		channel document so that it has a sensible instrument field.  
%%		instead it makes my code hideous :(.
%% @todo this needs to be broken up into multiple functions
%% @end
%%---------------------------------------------------------------------%%
normalize_instrument_name(Value,In) ->
	InstrDoc = couchbeam_doc:get_value(<<"instrument">>,Value),
	Result = case dict:find(InstrDoc,In) of
		{ok, Value2} ->
			ModName = binary_to_atom(couchbeam_doc:get_value(<<"instrument_model">>,Value2)),
			InstrName = binary_to_atom(couchbeam_doc:get_value(<<"name">>,Value2)),
			Locator = couchbeam_doc:get_value(<<"locator">>,Value),
			dict:from_list([
				{id,InstrName},
				{module,ModName},
				{locator,Locator}
			]);
		error ->
			{error, {bad_instrument, InstrDoc}}
	end,
	Result.

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