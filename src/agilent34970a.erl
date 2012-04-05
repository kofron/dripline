-module(agilent34970a).
-behaviour(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([read/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%
%%% Macros etc %%%
%%%%%%%%%%%%%%%%%%
-type channel_spec() :: {integer(),integer()}.
-type id_type() :: atom().
-type bus_type() :: atom().
-type addr_type() :: integer().
-define(DEFAULT_CACHE_EXP,20000).
-define(READ_ACC_TMO,200).
-record(cache_value, {last,ttl,ts}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {
	  q,
	  cache,
	  tref,
	  id :: id_type(),
	  epro_handle :: bus_type(),
	  gpib_addr :: addr_type(),
	  read_cmd :: fun(),
	  write_cmd :: fun()
}).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
read(InstrumentID, Locator) ->
    gen_server:call(InstrumentID ,{read, Locator}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(InstrumentID,PrologixID,BusAddress) ->
    Args = [InstrumentID,PrologixID,BusAddress],
    gen_server:start_link({local, InstrumentID}, ?MODULE, Args, []).

init([InstrumentID,PrologixID,BusAddress]) ->
    Locators = infer_init_scan_list(),
    InitCmds = setup_cmds(Locators),
    {InitCmds, TRef} = case Locators of
			   [] ->
			       {setup_cmds(Locators), none};
			   SomeLocators ->
			       BaseCmds = setup_cmds(Locators),
			       {[BaseCmds,";:INIT"],init_cache_expiry(100)}
	   end,
    InitialState = #state{
      id = InstrumentID,
      gpib_addr = BusAddress,
      epro_handle = PrologixID,
      q = [],
      tref = TRef,
      cache = dict:new(),
      read_cmd = fun(X) ->
			 eprologix_cmdr:send(PrologixID,
					     BusAddress,
					     X)
		 end,
      write_cmd = fun(X) ->
			  eprologix_cmdr:send(PrologixID,
					      BusAddress,
					      X,
					      true)
		  end
     },
    instrument_send(InitCmds,InitialState#state.write_cmd),
    {ok, InitialState}.

handle_call({read, Locator}, From, #state{tref=none,q=Q}=State) ->
    NewQ = append_action({read, Locator, From},Q),
    NewState = State#state{
		 q = NewQ
		},
    {noreply, NewState, ?READ_ACC_TMO};
handle_call({read, Locator}, From, #state{cache=C,tref=TRef,q=Q}=State) ->
    Branch = case fetch_last_value(Locator,C) of
		 {cache_good, Value} ->
		     {reply, Value, State};
		 new_channel ->
		     erlang:cancel_timer(TRef), % stop the current timer
		     NewQ = append_action({read, Locator, From},Q),
		     NewState = State#state{
				  tref = none,
				  q = NewQ
				 },
		     {noreply, NewState, ?READ_ACC_TMO}
	     end,
    Branch.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(timeout, #state{q=Q,cache=C,read_cmd=R,write_cmd=W}=State) ->
    Opts = [{reader,R},{writer,W}],
    NewCache = augment_and_update_cache(Q,C,Opts),
    respond(Q,NewCache),
    TRef = init_cache_expiry(),
    NewState = State#state{
		 cache = NewCache,
		 q = [],
		 tref = TRef
		},
    {noreply, NewState};    
handle_info(update_cache, #state{cache=C,read_cmd=F}=State) ->
    NewCache = update_cache(C,[{reader,F}]),
    TRef = init_cache_expiry(),
    NewState = State#state{
		 cache = NewCache,
		 tref = TRef
		},
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
infer_init_scan_list() ->
    [].

init_cache_expiry() ->
    erlang:send_after(?DEFAULT_CACHE_EXP,self(),update_cache).
init_cache_expiry(TMO_MS) ->
    erlang:send_after(TMO_MS,self(),update_cache).

append_action(NewAction, OldQueue) ->
    [NewAction|OldQueue].

respond([], _Cache) ->
    ok;
respond([{write, _Loc, _Val, _From}|T], Cache) ->
    respond(T,Cache);
respond([{read, Loc, From}|T], Cache) ->
    #cache_value{last=V} = dict:fetch(Loc,Cache),
    gen_server:reply(From, V),
    respond(T,Cache).

augment_and_update_cache([],Cache,Opts) ->
    Opt = [{new_ch, true}|Opts],
    update_cache(Cache,Opt);
augment_and_update_cache([{read, Loc, _From}|T],C,Opts) ->
    V = new_cache_value(Loc),
    NewCache = dict:store(Loc,V,C),
    augment_and_update_cache(T,NewCache,Opts);
augment_and_update_cache([{write, Loc, _Val, _From}|T],C,Opts) ->
    V = new_cache_value(Loc),
    NewCache = dict:store(Loc,V,C),
    augment_and_update_cache(T,NewCache,Opts).

update_cache(Cache,Options) ->
    Rd = proplists:get_value(reader,Options),
    Wr = proplists:get_value(writer,Options),
    Result = case proplists:get_value(new_ch,Options) of
		 true ->
		     ScanCmd = sl_cmd(sl_from_cache(Cache)),
		     TrigCmd = trig_cmd(),
		     TimeCmd = timing_cmd(smallest_ttl(Cache)),
		     ReadCmd = readback_cmd(dict:size(Cache)),
		     Cmd = [ScanCmd,";:",TrigCmd,";:",TimeCmd,";:","INIT"],
		     ok = instrument_send(Cmd,Wr),
		     timer:sleep(500),
		     instrument_send(ReadCmd,Rd);
		 undefined ->
		     instrument_send(readback_cmd(dict:size(Cache)),Rd)
	     end,
    ParsedResult = parse_instrument_response(Result),
    refresh_cache(ParsedResult, Cache).

setup_cmds(Locators) ->
    SL = channel_spec_list_to_scan_string(Locators),
    [
     "*CLS;",
     "*RST;:",
     sl_cmd(SL),
     ";",
     ":FORM:READ:CHAN ON;",
     ":FORM:READ:TIME ON;", 
     ":FORM:READ:TIME:TYPE ABS;",
     ":FORM:READ:UNIT ON"
    ].

trig_cmd() ->
    [
     "ABOR"
     ";:",
     "TRIG:SOURCE BUS",
     ";:",
     "INIT",
     ";"
     "*TRG"
     ";:",
     "ABOR"
    ].

instrument_send(Cmd, Fun) ->
    Fun(Cmd).

parse_instrument_response(Bin) ->
    parse_instrument_response(Bin, []).
parse_instrument_response(<<"\n">>, Acc) ->
    Acc;
parse_instrument_response(<<V:152/bitstring,
			    ",",
			    Y:32/bitstring,
			    ",",
			    M:16/bitstring,
			    ",",
			    D:16/bitstring,
			    ",",
			    HH:16/bitstring,
			    ",",
			    MM:16/bitstring,
			    ",",
			    SS:16/bitstring,
			    ".",
			    _MS:24/bitstring,
			    ",",
			    Ch:24/bitstring,
			    Rest/binary>>, Acc) ->
    Locator = locator_from_binary(Ch),
    Ts = <<Y/binary,"-",M/binary,"-",D/binary,
	   " ", 
	   HH/binary, MM/binary, SS/binary>>,
    R = (new_cache_value(Locator))#cache_value{
	  last = V,
	  ts = Ts
	 },
    parse_instrument_response(Rest,[{Locator,R}|Acc]).

new_cache_value(_Locator) ->
    #cache_value{
		 last = <<>>,
		 ttl = ?DEFAULT_CACHE_EXP
		}.

refresh_cache([],Cache) ->
    Cache;
refresh_cache([{Loc,#cache_value{last=V,ts=Ts}}|T],Cache) ->
    ValUpdater = fun(#cache_value{}=CV) ->
			 CV#cache_value{last=V,ts=Ts}
		 end,
    NewCache = dict:update(Loc,ValUpdater,Cache),
    refresh_cache(T,NewCache).    

fetch_last_value(Locator, Cache) ->
    Result = case dict:find(Locator,Cache) of
		 {ok, #cache_value{last=Value}} ->
		     {cache_good, Value};
		 error ->
		     new_channel
	     end,
    Result.

locator_from_binary(Bin) ->
    {I,[]} = string:to_integer(binary:bin_to_list(Bin)),
    {I div 100, I rem 100}.

smallest_ttl(Cache) ->
    KeepSmallest = fun(_K,#cache_value{ttl=T},A1) when T < A1 -> T;
		      (_,_,A1) -> A1 end,
    dict:fold(KeepSmallest, ?DEFAULT_CACHE_EXP, Cache).

sl_from_cache(Cache) ->
    channel_spec_list_to_scan_string(dict:fetch_keys(Cache)).

sl_cmd(ScanList) ->    
    io_lib:format("ROUT:SCAN (@~s)",[ScanList]).

timing_cmd(Interval) when is_integer(Interval) ->
    ISec = Interval div 1000,
    [
     "TRIG:SOURCE TIMER",
     ";:",
     io_lib:format("TRIG:TIMER ~B",[ISec]),
     ";:",
     "TRIG:COUNT INFINITY"
    ].

readback_cmd(NumChannels) when is_integer(NumChannels) ->
    io_lib:format("DATA:REMOVE? ~B",[NumChannels]).

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
