%% @doc the agilent 34970a is a "data acquisition/switch unit", which 
%% 		means it carries a number of multifunction modules, such as
%%		ADC banks, general purpose switches, RF muxers, and so on.  
%%		They are controlled via either GPIB or RS232 - currently the
%% 		module only supports the GPIB interface.  because the unit is
%%		armature based, we need to not hit it very frequently.  this
%%		module is a finite state machine that uses a cache invalidation
%%		scheme to minimize the trips to hardware that we need to make.
%% @author jared kofron <jared.kofron@gmail.com>
-module(agilent34970a_fsm).
-behavior(gen_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% External API %%%
%%%%%%%%%%%%%%%%%%%%
-export([read/2]).
-export([locator_to_ch_spec/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% data types and records %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type locator() :: binary().
-type channel_spec() :: {integer(),integer()}.
-type cache_type() :: dict().
-type cache_data_type() :: {binary(), calendar:datetime()}.
-type result_type() :: binary().
-type id_type() :: atom().
-type bus_type() :: atom().
-type addr_type() :: integer().

-record(request,{
			ts :: calendar:datetime(),
			loc :: locator(),
			from :: {pid(), term()},
			result :: result_type()
	}).

-record(scan_list,{
			raw :: [channel_spec()],
			len :: integer(),
			parsed :: string()
	}).

-record(cache_value,{
			last :: cache_data_type(),
			ttl :: integer,
			slist :: [channel_spec()]
	}).

-opaque request() :: #request{}.
-opaque scan_list() :: #scan_list{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal fsm state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{
			id :: id_type(),
			epro_handle :: bus_type(),
			gpib_addr :: addr_type(),
			c_req :: request(),
			slist :: scan_list(), % channels to be scanned
			ival :: integer(), % scanning interval in seconds
			cache
	}).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
-export([configuring/2]).
-export([waiting/2]).
-export([validate_cache/2]).
-export([update_cache/2]).
-export([update_scan_list/2]).
-export([update_instrument/2]).
-export([refresh_cache/2]).
-export([fetch_cached_value/2]).
-export([shipping/2]).

%%%%%%%%%%%%%%
%%% Macros %%%
%%%%%%%%%%%%%%
-define(NOW,0).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc read/2 maps a request for data onto the correct instrument and
%%		synchronously returns either data or a descriptive error.  Error
%%		codes that are returned are from the instrument itself.
%% @end
%%---------------------------------------------------------------------%%
-spec read(atom(),locator()) -> binary() | {error, term()}.
read(InstrumentID,Locator) ->
	case locator_to_ch_spec(Locator) of
		{error, _Reason}=Error ->
			Error;
		{_Card,_Channel}=CSpec ->
			gen_fsm:sync_send_all_state_event(InstrumentID,{read,CSpec})
	end.

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
-spec locator_to_ch_spec(binary()) -> 
		channel_spec() | {error, bad_locator}.
locator_to_ch_spec(LocatorBinary) ->
	LocatorString = erlang:binary_to_list(LocatorBinary),
	case io_lib:fread("{~u,~u}",LocatorString) of
		{ok, [CardNumber,ChannelNumber], []} ->
			{CardNumber,ChannelNumber};
		_Error ->
			{error, bad_locator}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(InstrumentID,PrologixID,BusAddress) ->
	Args = [InstrumentID,PrologixID,BusAddress],
	gen_fsm:start_link({local,InstrumentID},?MODULE,Args,[]).

init([InstrumentID,PrologixID,BusAddress]) ->
	InitialState = #state{
		id = InstrumentID,
		gpib_addr = BusAddress,
		epro_handle = PrologixID,
		c_req = none,
		cache = dict:new(),
		slist = #scan_list{
			raw = [],
			parsed = ""
		},
		ival = 5
	},
	{ok, configuring, InitialState, ?NOW}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event({read,CSpec}, From, waiting, StateData) ->
	Req = #request{
		ts = calendar:local_time(),
		loc=CSpec,
		from=From
	},
	NewStateData = StateData#state{
		c_req = Req
	},
	{next_state, validate_cache, NewStateData, ?NOW}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm state definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
configuring(timeout,#state{gpib_addr=A,epro_handle=H}=StateData) ->
	lists:foreach(fun(C) -> 
					eprologix_cmdr:send(H,A,C,true),
					timer:sleep(1000)
					end,
					startup_config_strings()
				 ),
	{next_state,waiting,StateData}.

waiting(timeout,StateData) ->
	{next_state, validate_cache, StateData, ?NOW}.

validate_cache(timeout, #state{c_req=R,cache=C}=StateData) ->
	NextState = case is_expired(C,R) of
			{true, new_channel} ->
				update_cache;
			true ->
				refresh_cache;
			false ->
				fetch_cached_value
	end,
	{next_state,NextState,StateData,?NOW}.

update_cache(timeout, #state{c_req=R,cache=C}=StateData) ->
	CachedValue = #cache_value{
		last = {<<>>,never},
		ttl = 20
	},
	ChannelName = R#request.loc,
	NewCache = dict:store(ChannelName,CachedValue,C),
	NewStateData = StateData#state{
		cache = NewCache
	},
	{next_state, update_scan_list, NewStateData, ?NOW}.

update_scan_list(timeout, #state{epro_handle=H,gpib_addr=A,c_req=R,slist=S}=StateData) ->
	ChannelName = R#request.loc,
	NewChannelList = [ChannelName|S#scan_list.raw],
	NewStateData = StateData#state{
		slist = #scan_list{
			raw = NewChannelList,
			len = erlang:length(raw),
			parsed = channel_spec_list_to_scan_string(NewChannelList)
		}
	},
	Cmd = scan_list_command((NewStateData#state.slist)#scan_list.parsed),
	eprologix_cmdr:send(H,A,Cmd,true),
	{next_state, update_instrument, NewStateData, ?NOW}.

update_instrument(timeout, #state{epro_handle=H,gpib_addr=A,ival=I}=StateData) ->
	lists:foreach(fun(C) ->
						eprologix_cmdr:send(H,A,C,true),
						timer:sleep(100)
					end,
				instrument_timer_commands(I)),
	{next_state, refresh_cache, StateData, ?NOW}.

refresh_cache(timeout, #state{gpib_addr=A,epro_handle=H,cache=C,slist=#scan_list{len=L}}=StateData) ->
	Cmd = io_lib:format("DATA:REMOVE? ~B",[L]),
	RawData = eprologix_cmdr:send(H,A,Cmd),
	io:format("~p~n",[RawData]),
	UpdateTS = fun(K,#cache_value{last={D,_}}=Cached) ->
						NewTS = calendar:local_time(),
						Cached#cache_value{last={D,NewTS}}
				end,
	NewCache = dict:map(UpdateTS,C),
	NewStateData = StateData#state{
		cache = NewCache
	},
	{next_state, fetch_cached_value, NewStateData, ?NOW}.

fetch_cached_value(timeout, #state{c_req=R,cache=C}=StateData) ->
	ChannelName = R#request.loc,
	#cache_value{last={Value,_Time}} = dict:fetch(ChannelName,C),
	NewRequest = R#request{
		result = Value
	},
	NewStateData = StateData#state{
		c_req = NewRequest
	},
	{next_state, shipping, NewStateData, ?NOW}.

shipping(timeout, #state{c_req=R}=StateData) ->
	To = R#request.from,
	Ans = R#request.result,
	gen_fsm:reply(To,Ans),
	{next_state,waiting,StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc is_expired/2 inspects the fsm cache wrt the time that the request
%%		is made.  if the channel ttl is shorter than the time between now
%%		and the request, then the cache is bad and needs to be refreshed.
%% @end
%%---------------------------------------------------------------------%%
-spec is_expired(cache_type(),request()) -> true | false.
is_expired(Cache,#request{loc = L, ts=T}) ->
	case dict:find(L,Cache) of
		{ok, CachedValue} ->
			should_be_refreshed(CachedValue,T);
		error ->
			{true, new_channel}
	end.

%%---------------------------------------------------------------------%%
%% @doc should_be_refreshed/2 takes a cached value and a timestamp and
%%		returns true or false based on the last time the channel was read
%%		and the channel time-to-live.
%% @end
%%---------------------------------------------------------------------%%
-spec should_be_refreshed(cache_data_type(),calendar:datetime()) 
		-> boolean().
should_be_refreshed(#cache_value{last={_LastVal,never},ttl=_Dt},_T) ->
	true;
should_be_refreshed(#cache_value{last={_LastVal,T0},ttl=Dt},T) ->
	case calendar:time_difference(T0,T) of
		{0,{_,_,_}=Elapsed} ->
			case calendar:time_to_seconds(Elapsed) of
				D when D > Dt ->
					true;
				_D ->
					false
			end;
		{_,_}=X ->
			true
	end.

%%---------------------------------------------------------------------%%
%% @doc channel_spec_to_int/1 takes a channel specifier 
%%		{CardNumber,ChannelNumber} and converts it to the data that the 
%%		switch unit uses for addressing, which is a single integer.
%% @end
%%---------------------------------------------------------------------%%
-spec channel_tuple_to_int(channel_spec()) -> integer().
channel_tuple_to_int({CardNumber,ChannelNumber}) ->
	100*CardNumber + ChannelNumber.

%%---------------------------------------------------------------------%%
%% @doc channel_spec_list_to_scan_string is a very fun function.  
%%		it takes a list of 34970a channel specs and produces a
%%		string that the agilent 34970a is expecting, which is of the form
%%		"101,105,301...".  the cool part is range detection.  the 
%%		instrument takes ranges in the form of 101:105, which means all
%%		channels between 1 and 5 on card 1.  this function will 
%%		automagically produce the appropriate range queries if ranges
%%		are found in the list of tuples.
%% @end
%%---------------------------------------------------------------------%%
-spec channel_spec_list_to_scan_string([channel_spec()]) -> string().
channel_spec_list_to_scan_string([]) ->
	"";
channel_spec_list_to_scan_string({_,_}=SingleTuple) ->
	channel_spec_list_to_scan_string([SingleTuple]);
channel_spec_list_to_scan_string(Tuples) ->
	IntChannels = [channel_tuple_to_int(Y) || Y <- Tuples],
	channel_ints_to_scan_string(lists:sort(IntChannels),false,[]).

%%---------------------------------------------------------------------%%
%% @doc channel_ints_to_scan_string is where the magic happens in terms
%%		of generating the scan list for the agilent 34970a.
%% @see channel_spec_list_to_scan_string
%% @end
%%---------------------------------------------------------------------%%
-spec channel_ints_to_scan_string([channel_spec()],atom(),string()) ->
		string().
channel_ints_to_scan_string([], false, Acc) ->
	lists:flatten(lists:reverse(Acc));
channel_ints_to_scan_string([], H0, Acc) ->
	Str = io_lib:format(":~p",[H0]), 
	lists:reverse([Str|Acc]);

channel_ints_to_scan_string([H1,H2|T],false,Acc) when H2 == (H1 + 1) ->
	Str = io_lib:format("~p",[H1]),
	channel_ints_to_scan_string(T,H2,[Str|Acc]);

channel_ints_to_scan_string([H|T],H0,Acc) when H == (H0 + 1) ->
	channel_ints_to_scan_string(T,H,Acc);

channel_ints_to_scan_string([S],false,Acc) ->
	Str = io_lib:format("~p",[S]),
	channel_ints_to_scan_string([],false,[Str|Acc]);

channel_ints_to_scan_string([H|T],false,Acc) ->
	Str = io_lib:format("~p",[H]),
	channel_ints_to_scan_string(T,false,[Str ++ ","|Acc]);

channel_ints_to_scan_string([_H|_T]=L,H0,Acc) ->
	Str = io_lib:format(":~p",[H0]),
	channel_ints_to_scan_string(L,false,[Str ++ ","|Acc]).

startup_config_strings() ->
	[
		"*CLS",
		"*RST",
		"FORM:READ:CHAN ON",
		"FORM:READ:TIME ON",
		"FORM:READ:UNIT ON"
	].

scan_list_command(ScanList) ->
	io_lib:format("ROUT:SCAN (@~s)",[ScanList]).

instrument_timer_commands(I) ->
	[
		"TRIG:SOURCE TIMER",
		io_lib:format("TRIG:TIMER ~B",[I]),
		"TRIG:COUNT INFINITY",
		"INIT"
	].

%%%%%%%%%%%%%%%
%%% Testing %%%
%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_string_input() ->
	[{1,1},{1,3},{1,5},{1,6},{1,7},{1,8},{1,19},{2,10},
	{2,12},{2,13},{2,14},{2,15},{3,1},{3,3},{3,8},{3,20}].
scan_string_result() ->
	"101,103,105:108,119,210,212:215,301,303,308,320".
scan_string_test() ->
	In = scan_string_input(),
	Out = scan_string_result(),
	?assertEqual(Out,channel_spec_list_to_scan_string(In)).
-endif.