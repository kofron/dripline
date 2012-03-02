%% @doc the agilent 34970a is a "data acquisition/switch unit", which 
%% 		means it carries a number of multifunction modules, such as
%%		ADC banks, general purpose switches, RF muxers, and so on.  
%%		They are controlled via either GPIB or RS232 - currently the
%% 		module only supports the GPIB interface.
%% @author jared kofron <jared.kofron@gmail.com>
%% @todo write/configure functions
-module(agilent34970a).
-behavior(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([read/2]).
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
-type channel_spec() :: {integer(),integer()}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc read/2 maps a request for data onto the correct instrument and
%%		synchronously returns either data or a descriptive error.  Error
%%		codes that are returned are from the instrument itself.
%% @end
%%---------------------------------------------------------------------%%
-spec read(atom(),channel_spec()) -> binary() | {error, term()}.
read(InstrumentID,ChannelSpec) ->
	gen_server:call(InstrumentID,{read,ChannelSpec}).

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
		channel_spec() | {error, bad_locator}.
locator_to_ch_data(LocatorBinary) ->
	LocatorString = erlang:binary_to_list(LocatorBinary),
	case io_lib:fread("{~u,~u}",LocatorString) of
		{ok, [CardNumber,ChannelNumber], []} ->
			{CardNumber,ChannelNumber};
		_Error ->
			{error, bad_locator}
	end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%---------------------------------------------------------------------%%
%% @doc start_link/3 starts the instrument interface with a specific 
%%		prologix device and bus address.  note that we are just assuming
%%		that we can actually talk to the bus - init/1 succeeds no matter
%%		what.
%% @todo maybe we shouldn't succeed if the bus isn't available.
%% @end
%%---------------------------------------------------------------------%%
start_link(InstrumentID,PrologixID,BusAddress) ->
	Args = [InstrumentID,PrologixID,BusAddress],
	gen_server:start_link({local, InstrumentID}, ?MODULE, Args, []).

init([InstrumentID,PrologixID,BusAddress]) ->
	InitialState = #state{
		gpib_addr=BusAddress,id=InstrumentID,epro_handle=PrologixID
	},
	{ok, InitialState}.

handle_call({read,Channels}, From, #state{epro_handle = H}=StateData) ->
	ReadStr = read_channel_string(Channels),
	AddrStr = "++addr 9\n",
	{ok, R} = eprologix_cmdr:send_query(H,[AddrStr|ReadStr]),
	OutgoingReq = #req_data{from=From,ref = R},
	NewStateData = StateData#state{c_req = OutgoingReq},
	{noreply, NewStateData}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({R,Data}, #state{c_req=#req_data{from=F,ref=R}}=StateData) ->
	gen_server:reply(F,Data),
	NewStateData = StateData#state{c_req=none},
	{noreply, NewStateData}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc channel_tuple_to_int/1 takes a tuple {CardNumber,ChannelNumber}
%% 		and converts it to the data that the agilent is expecting, which
%%		is a single integer.
%% @end
%%---------------------------------------------------------------------%%
-spec channel_tuple_to_int({integer(),integer()}) -> integer().
channel_tuple_to_int({CardNumber,ChannelNumber}) ->
	100*CardNumber + ChannelNumber.

%%---------------------------------------------------------------------%%
%% @doc channel_tuple_list_to_channel_spec is a very fun function.  
%%		it takes a list of tuples [{integer(),integer()}] and produces a
%%		string that the agilent 34970a is expecting, which is of the form
%%		"101,105,301...".  the cool part is range detection.  the 
%%		instrument takes ranges in the form of 101:105, which means all
%%		channels between 1 and 5 on card 1.  this function will 
%%		automagically produce the appropriate range queries if ranges
%%		are found in the list of tuples.
%% @end
%%---------------------------------------------------------------------%%
-spec channel_tuple_list_to_channel_spec([channel_spec()]) -> string().
channel_tuple_list_to_channel_spec([]) ->
	"";
channel_tuple_list_to_channel_spec(Tuples) ->
	IntChannels = [channel_tuple_to_int(Y) || Y <- Tuples],
	channel_ints_to_channel_spec(lists:sort(IntChannels),false,[]).

%%---------------------------------------------------------------------%%
%% @doc channel_ints_to_channel_spec is where the magic happens in terms
%%		of generating the actual data to be sent to the instrument.  
%% @see channel_tuple_list_to_channel_spec
%% @end
%%---------------------------------------------------------------------%%
-spec channel_ints_to_channel_spec([channel_spec()],atom(),string()) ->
		string().
channel_ints_to_channel_spec([], false, Acc) ->
	lists:reverse(Acc);
channel_ints_to_channel_spec([], H0, Acc) ->
	Str = io_lib:format(":~p",[H0]), 
	lists:reverse([Str|Acc]);

channel_ints_to_channel_spec([H1,H2|T],false,Acc) when H2 == (H1 + 1) ->
	Str = io_lib:format("~p",[H1]),
	channel_ints_to_channel_spec(T,H2,[Str|Acc]);

channel_ints_to_channel_spec([H|T],H0,Acc) when H == (H0 + 1) ->
	channel_ints_to_channel_spec(T,H,Acc);

channel_ints_to_channel_spec([S],false,Acc) ->
	Str = io_lib:format("~p",[S]),
	channel_ints_to_channel_spec([],false,[Str|Acc]);

channel_ints_to_channel_spec([H|T],false,Acc) ->
	Str = io_lib:format("~p",[H]),
	channel_ints_to_channel_spec(T,false,[Str ++ ","|Acc]);

channel_ints_to_channel_spec([_H|_T]=L,H0,Acc) ->
	Str = io_lib:format(":~p",[H0]),
	channel_ints_to_channel_spec(L,false,[Str ++ ","|Acc]).

%%---------------------------------------------------------------------%%
%% @doc read_channel_string takes a string and returns the command string
%%		to be sent over GPIB to read from the instrument.
%% @end
%%---------------------------------------------------------------------%%
-spec read_channel_string(string()) -> string().
read_channel_string(ChannelList) ->
	ChannelSpec = channel_tuple_list_to_channel_spec(ChannelList),
	"MEAS:VOLT:DC? 10,0.003, (@" ++ ChannelSpec ++ ")".
