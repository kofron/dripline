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
-spec lookup(atom(),binary()) -> {ok,term()} | {error,term()}.
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

handle_call({lookup,{ch,Name}}, _From, #state{chs=Ch,ins=In}=StateData) ->
	Reply = case dict:find(Name,Ch) of
		{ok, Value} ->
			Value;
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

%%---------------------------------------------------------------------%%
%% @doc generate_channel_dict/1 generates a dictionary from a list of 
%%		channels.  this dict is used internally by the server to find 
%%		channel data quickly.
%% @end
%%---------------------------------------------------------------------%%
generate_channel_dict(ChannelViewResults) ->
	generate_dict_from_kv_list(ChannelViewResults,dict:new()).

%%---------------------------------------------------------------------%%
%% @doc generate_instrument_dict/1 generates a dictionary from a list of
%%		instruments.  this dict is used internally by the server to find
%%		instrument data quickly, and also to generate the functions 
%%		needed to resolve data requests.
%% @end
%%---------------------------------------------------------------------%%
generate_instrument_dict(InstrumentViewResults) ->
	generate_dict_from_kv_list(InstrumentViewResults,dict:new()).

%%---------------------------------------------------------------------%%
%% @doc generate_dict_from_kv_list/2 doesn't _quite_ do what it says -
%%		really it generates a dict from a couch document.  it is 
%%		essentially an abstracted version of generate_channel_dict.
%% @todo change the name of this to be more accurate.
%%---------------------------------------------------------------------%%
generate_dict_from_kv_list([],Acc) ->
	Acc;
generate_dict_from_kv_list([H|T],Acc) ->
	K = couchbeam_doc:get_value(<<"key">>,H),
	V = couchbeam_doc:get_value(<<"value">>,H),
	generate_dict_from_kv_list(T,dict:store(K,V,Acc)).

%%---------------------------------------------------------------------%%
%% @doc binary_to_atom simply converts a binary string into an atom.
%% @end
%%---------------------------------------------------------------------%%
binary_to_atom(Binary) ->
	erlang:list_to_atom(erlang:binary_to_list(Binary)).