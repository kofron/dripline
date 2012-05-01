%% @doc The Agilent E3631A is a triple output laboratory power supply.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(agilente3631a).
-behavior(gen_server).

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

%%%%%%%%%%%%%%%%%%
%%% Data Types %%%
%%%%%%%%%%%%%%%%%%
-type locator() :: binary().
-type channel_spec() :: string().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {id, gpib_addr, epro_handle}).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec locator_to_ch_spec(locator()) -> 
				channel_spec() | {error, bad_locator}.
locator_to_ch_spec(<<"6V">>) ->
    "P6V";
locator_to_ch_spec(<<"25V+">>) ->
    "P25V";
locator_to_ch_spec(<<"25V-">>) ->
    "N25V";
locator_to_ch_spec(_) ->
    {error, bad_locator}.

-spec read(atom(),locator()) -> binary() | {error, term()}.
read(InstrumentID, Locator) ->
    case locator_to_ch_spec(Locator) of
	{error, bad_locator}=E ->
	    E;
	CSpec ->
	    gen_server:call(InstrumentID,{read,CSpec})
    end.

-spec write(atom(),locator(),binary()) -> ok | {error, term()}.
write(InstrumentID, Locator, Value) ->
    case unpack_value(Locator, Value) of
	{ok, Val} ->
	    CSpec = locator_to_ch_spec(Locator),
	    gen_server:call(InstrumentID, {write, {CSpec, Val}});
	{error, _Rsn}=E ->
	    E
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

handle_call({read, Ch}, _From, #state{epro_handle=H, gpib_addr=A}=SD) ->
    ReadStr = ["MEAS:VOLT? ",Ch,";:","MEAS:CURR? ",Ch],
    Result = eprologix_cmdr:send(H,A,ReadStr),
    RetVal = pack_data(ok, Result, dripline_util:make_ts()),
    {reply, RetVal, SD};
handle_call({write, {Ch, Val}}, _From, #state{epro_handle=H, gpib_addr=A}=SD) ->
    WriteStr = case Val of
		   <<"ON">> ->
		       ["INST:SEL ",Ch,";:","OUTP:STAT ",Val];
		   <<"OFF">> ->
		       ["INST:SEL ",Ch,";:","OUTP:STAT ",Val];
		   Val ->
		       ["APPL ",Ch,",",Val]
	       end,
    eprologix_cmdr:send(H,A,WriteStr,true),
    RetVal = pack_data(ok, ok, dripline_util:make_ts()),
    {reply, RetVal, SD}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc pack_data generates an appropriate instrument dripline_data 
%%      response.
%%---------------------------------------------------------------------%%
-spec pack_data(atom(), binary(), binary()) -> dripline_data:dl_data().
pack_data(ok, Data, Timestamp) ->
    R0 = dripline_data:new(),
    R1 = dripline_data:set_ts(R0, Timestamp),
    R2 = dripline_data:set_code(R1, ok),
    dripline_data:set_data(R2, Data).

%%---------------------------------------------------------------------%%
%% @doc unpack_value converts a value passed in from the database 
%%---------------------------------------------------------------------%%
-spec unpack_value(locator(), binary()) -> 
			  {ok, binary()} | {error, term()}.
unpack_value(_Loc, <<"enable">>) ->
    {ok, <<"ON">>};
unpack_value(_Loc, <<"disable">>) ->
    {ok, <<"OFF">>};
unpack_value(_Loc, Bin) ->
    case binary:match(Bin,<<",">>) of
	nomatch ->
	    parse_const_mode(Bin);
	_Match ->
	    parse_reg_mode(Bin)
    end.
	    
-spec parse_const_mode(binary()) -> binary().
parse_const_mode(B) ->
    case {is_v(B), is_c(B)} of
	{true, false} ->
	    {ok, [drop_v(B),",",<<"MAX">>]};
	{false, true} ->
	    {ok, [<<"MAX">>,",",drop_c(B)]};
        _AnyOther ->
	    {error, {badval, B}}
    end.

-spec parse_reg_mode(binary()) -> binary().
parse_reg_mode(B) ->
    [V,C] = binary:split(B,<<",">>),
    case {is_v(V), is_c(V), is_v(C), is_c(C)} of
	{true, false, false, true} -> % Voltage, Current
	    {ok, [drop_v(V),",",drop_c(C)]};
	{false, true, false, true} -> % Current, Current
	    {error, {badval, B}};
	{false, true, true, false} -> % Current, Voltage
	    {ok, [drop_v(C),",",drop_c(V)]};
	{true, false, true, false} -> % Voltage, Voltage
	    {error, {badval, B}} ;
	_Other -> % Nonsense
	    {error, {badval,B}}
    end.

-spec is_v(binary()) -> boolean().
is_v(B) ->
    binary:last(B) == $V.

-spec is_c(binary()) -> boolean().
is_c(B) ->
    binary:last(B) == $A.

-spec drop_v(binary()) -> binary().
drop_v(Bin) ->
    binary:replace(Bin,<<"V">>,<<>>).

-spec drop_c(binary()) -> binary().
drop_c(Bin) ->
    binary:replace(Bin,<<"A">>,<<>>).
