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

-record(request,{
			ts :: calendar:datetime(),
			loc :: locator(),
			from :: {pid(), term()},
			result :: result_type()
	}).

-record(cache_value,{
			last :: cache_data_type(),
			ttl :: integer
	}).

-opaque request() :: #request{}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal fsm state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{
			c_req :: request(),
			cache
	}).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
-export([waiting/2]).
-export([validate_cache/2]).
-export([update_cache/2]).
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
	ChannelSpec = locator_to_ch_spec(Locator),
	gen_fsm:sync_send_all_state_event(InstrumentID,{read,ChannelSpec}).

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
	gen_fsm:start_link({local,?MODULE},?MODULE,Args,[]).

init([InstrumentID,PrologixID,BusAddress]) ->
	InitialState = #state{
		c_req = none,
		cache = dict:new()
	},
	{ok, waiting, InitialState}.

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
	{next_state, refresh_cache, NewStateData, ?NOW}.

refresh_cache(timeout, #state{c_req=_R,cache=C}=StateData) ->
	%% go get a new cache... how about we just sleep for now?
	ok = timer:sleep(1000),
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


