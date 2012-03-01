-module(dripline_conf_mgr).
-behavior(gen_server).

%% API
-export([start_link/0]).
-export([lookup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal state
-record(state,{
			channels, 
			instruments
		}).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
lookup(channel,ChName) ->
	gen_server:call(?MODULE,{lookup,{ch,ChName}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	SConn = dripline_conn_mgr:get(),
	{ok, Db} = couchbeam:open_db(SConn,"dripline_conf"),
	{ok, AllInstruments} = couchbeam_view:fetch(Db,{"objects","instruments"}),
	{ok, AllChannels} = couchbeam_view:fetch(Db,{"objects","channels"}),
	ChanDict = generate_channel_dict(AllChannels),
	InstDict = generate_instrument_dict(AllInstruments),
	InitialState = #state{
		channels = ChanDict,
		instruments = InstDict
	},
	{ok, InitialState}.

handle_call({lookup,{ch,Name}}, _From, #state{channels=Ch,instruments=In}=StateData) ->
	Reply = case dict:find(Name,Ch) of
		{ok, Value} ->
			normalize_instrument_name(Value,In);
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
normalize_instrument_name(Value,In) ->
	InstrDoc = couchbeam_doc:get_value(<<"instrument">>,Value),
	Result = case dict:find(InstrDoc,In) of
		{ok, Value2} ->
			ModName = binary_to_atom(couchbeam_doc:get_value(<<"instrument_model">>,Value2)),
			InstrName = couchbeam_doc:get_value(<<"name">>,Value2),
			Locator = couchbeam_doc:get_value(<<"locator">>,Value),
			dict:from_list([
				{id,InstrName},
				{locator,ModName:locator_to_ch_data(Locator)},
				{read, fun(X) -> ModName:read(binary_to_atom(InstrName),X) end}
			]);
		error ->
			{error, {bad_instrument, InstrDoc}}
	end,
	Result.
generate_channel_dict(ChannelViewResults) ->
	generate_dict_from_kv_list(ChannelViewResults,dict:new()).

generate_instrument_dict(InstrumentViewResults) ->
	generate_dict_from_kv_list(InstrumentViewResults,dict:new()).

generate_dict_from_kv_list([],Acc) ->
	Acc;
generate_dict_from_kv_list([H|T],Acc) ->
	K = couchbeam_doc:get_value(<<"key">>,H),
	V = couchbeam_doc:get_value(<<"value">>,H),
	generate_dict_from_kv_list(T,dict:store(K,V,Acc)).

binary_to_atom(Binary) ->
	erlang:list_to_atom(erlang:binary_to_list(Binary)).