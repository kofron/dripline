-module(hp8657b).

-behaviour(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([read/2,write/3]).
-export([locator_to_ch_spec/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal record definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(req_data, {from, ref}).
-record(state, {id, gpib_addr, epro_handle, c_req}).

%%%%%%%%%%%%%%%%%%
%%% Data Types %%%
%%%%%%%%%%%%%%%%%%
-type locator() :: binary().
-type channel_spec() :: string().

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc locator_to_ch_spec is part of a generic instrument interface.  
%%		essentially it allows us to take a string that is human readable
%%		and turn it into something that the instrument understands as 
%%		referring to a channel.  This is nice because in the database,
%%		the amount of information that the user needs to know about the
%%		instrument itself is reduced.
%% @todo maybe this should get moved into a behavior?
%% @end
%%---------------------------------------------------------------------%%
-spec locator_to_ch_spec(locator()) -> 
		channel_spec() | {error,bad_locator}.
locator_to_ch_spec(<<"power_level">>) -> "AP";
locator_to_ch_spec(<<"cw_freq">>) -> "FR";
locator_to_ch_spec(_Other) -> {error,bad_locator}.

%%---------------------------------------------------------------------%%
%% @doc read/2 maps a request for data onto the correct instrument and
%%		synchronously returns either data or a descriptive error.  Error
%%		codes that are returned are from the instrument itself.  Because
%%		these are externally facing functions, we use locators instead
%%		of channel specs.
%% @end
%%---------------------------------------------------------------------%%
-spec read(atom(),locator()) -> binary() | {error,term()}.
read(InstrumentID,Locator) ->
	CHSpec = locator_to_ch_spec(Locator),
	gen_server:call(InstrumentID,{read,CHSpec}).

%%---------------------------------------------------------------------%%
%% @doc write/3 maps a write request onto the correct instrument and 
%%		synchronously returns either an ok message or a descriptive 
%%		error tuple.  Error codes come from the instrument.
%% @end
%%---------------------------------------------------------------------%%
-spec write(atom(),locator(),binary()) -> ok | {error,term()}.
write(InstrumentID,Locator,NewValue) ->
	Value = unpack_value(NewValue),
	CHSpec = locator_to_ch_spec(Locator),
	gen_server:call(InstrumentID,{write,{CHSpec,Value}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(InstrumentID,BusID,InstrumentAddress) ->
	Args = [InstrumentID,BusID,InstrumentAddress],
	gen_server:start_link({local,InstrumentID},?MODULE,Args,[]).

init([InstrumentID,BusID,InstrumentAddress]) ->
	InitialState = #state{
		id = InstrumentID,
		gpib_addr = InstrumentAddress,
		epro_handle = BusID
	},
	{ok, InitialState}.

handle_call({read,Channels}, From, 
			#state{epro_handle = H, gpib_addr = A}=StateData) ->
	ReadStr = read_channel_string(Channels),
	R = eprologix_cmdr:send(H,A,ReadStr),
	{reply, R, StateData};

handle_call({write,{Channels,NewValue}}, From,
			#state{epro_handle = H, gpib_addr = A}=StateData) ->
	WriteStr = case NewValue of
		       <<"enable">> ->
			   <<"R3">>;
		       <<"disable">> ->
			   <<"R2">>;
		       _Other ->
			   write_channel_string(Channels,NewValue)
		   end,
	ok = eprologix_cmdr:send(H,A,WriteStr,true),
	{reply, ok, StateData}.

handle_cast(_Cast,StateData) ->
	{noreply, StateData}.

handle_info({R,Data}, #state{c_req=#req_data{from=F,ref=R}}=StateData) ->
	gen_server:reply(F,Data),
	NewStateData = StateData#state{c_req=none},
	{noreply, NewStateData}.

terminate(_Reason, _StateData) ->
	ok.

code_change(_OldVsn, StateData, _Extras) ->
	{ok,StateData}.

%%%%%%%%%%%%%%%%
%%% internal %%%
%%%%%%%%%%%%%%%%
-spec read_channel_string(string()) -> string().
read_channel_string(CHString) ->
	"OP" ++ CHString.

-spec write_channel_string(string(),string()) -> string().
write_channel_string(CHString,Value) ->
	CHString ++ Value ++ unit_string(CHString).

-spec unit_string(string()) -> string().
unit_string("FR") -> "MZ";
unit_string("AP") -> "DM".

-spec unpack_value(binary()) -> binary().
unpack_value(B) ->
    B.
%%%%%%%%%%%%%%%%%%%%%%%%%%
