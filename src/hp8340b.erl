%% @doc the Hewlett-Packard HP8340B is a high frequency synthesized 
%%		sweeper controlled via GPIB.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(hp8340b).
-behavior(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([read/2,write/3]).
-export([locator_to_ch_data/1]).

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
-type channel_spec() :: 'PL' | 'CW' | 'FA' | 'FB' | 'ST'.

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc locator_to_ch_data is part of a generic instrument interface.  
%%		essentially it allows us to take a string that is human readable
%%		and turn it into something that the instrument understands as 
%%		referring to a channel.  This is nice because in the database,
%%		the amount of information that the user needs to know about the
%%		instrument itself is reduced.
%% @todo maybe this should get moved into a behavior?
%% @end
%%---------------------------------------------------------------------%%
-spec locator_to_ch_data(binary()) -> 
		channel_spec() | {error,bad_locator}.
locator_to_ch_data(<<"power_level">>) -> 'PL';
locator_to_ch_data(<<"cw_freq">>) -> 'CW';
locator_to_ch_data(<<"sweep_start_freq">>) -> 'FA';
locator_to_ch_data(<<"sweep_stop_freq">>) -> 'FB';
locator_to_ch_data(<<"sweep_time">>) -> 'ST';
locator_to_ch_data(_Other) -> {error,bad_locator}.

%%---------------------------------------------------------------------%%
%% @doc read/2 maps a request for data onto the correct instrument and
%%		synchronously returns either data or a descriptive error.  Error
%%		codes that are returned are from the instrument itself.
%% @end
%%---------------------------------------------------------------------%%
-spec read(atom(),channel_spec()) -> binary() | {error,term()}.
read(InstrumentID,ChannelSpec) ->
	gen_server:call(InstrumentID,{read,ChannelSpec}).

%%---------------------------------------------------------------------%%
%% @doc write/3 maps a write request onto the correct instrument and 
%%		synchronously returns either an ok message or a descriptive 
%%		error tuple.  Error codes come from the instrument.
%% @end
%%---------------------------------------------------------------------%%
-spec write(atom(),channel_spec(),binary()) -> ok | {error,term()}.
write(InstrumentID,ChannelSpec,NewValue) ->
	gen_server:call(InstrumentID,{write,{ChannelSpec,NewValue}}).

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
	AddrStr = addr_string(A),
	{ok, R} = eprologix_cmdr:send_query(H,[AddrStr|ReadStr]),
	OutgoingReq = #req_data{from=From,ref = R},
	NewStateData = StateData#state{c_req = OutgoingReq},
	{noreply, NewStateData};

handle_call({write,{Channels,NewValue}}, From,
			#state{epro_handle = H, gpib_addr = A}=StateData) ->
	WriteStr = write_channel_string(Channels,NewValue),
	AddrStr = addr_string(A),
	{ok, R} = eprologix_cmdr:send_query(H,[AddrStr|WriteStr]),
	OutgoingReq = #req_data{from=From,ref = R},
	NewStateData = StateData#state{c_req = OutgoingReq},
	{noreply, NewStateData}.

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
-spec addr_string(integer()) -> string().
addr_string(N) ->
	NStr = lists:flatten(io_lib:format("~p",[N])),
	"++addr " ++ NStr ++ "\n".

-spec read_channel_string(string()) -> string().
read_channel_string(CHString) ->
	"OP" ++ CHString.

-spec write_channel_string(string(),string()) -> string().
write_channel_string(CHString,Value) ->
	CHString ++ Value ++ "GZ".
