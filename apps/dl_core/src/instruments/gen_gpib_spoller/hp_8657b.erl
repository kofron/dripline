%%%----------------------------------------------------------------------------
%%% @doc The HP 8657B is a low frequency sweeper which we use primarily as a local oscillaor reference frequency for our second receiver stage.
%%% @end
%%%----------------------------------------------------------------------------
-module(hp_8657b).
-behavior(gen_gpib_spoller).
-behavior(dl_gen_instrument).

-export([start_link/4,
         init/1,
         handle_stb/2,
         handle_esr/2,
         handle_get/2,
         handle_set/3,
         sre_register_bitmask/1,
         ese_register_bitmask/1]).

-record(state, {}).

-define(srq_asserted(X), ((X bsr 6) band 1) == 1).
-define(mav_asserted(X), ((X bsr 4) band 1) == 1).

%%%----------------------------------------------------------------------------
%%% gen_gpib_spoller callbacks
%%%----------------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

start_link(InstrName, GPIBAddress, BusMod, BusName) ->
    gen_gpib_spoller:start_link(?MODULE, InstrName, GPIBAddress, BusMod, BusName).

sre_register_bitmask(_) ->
    176.

ese_register_bitmask(_) ->
    1.

handle_stb(StatusByte, StateData) when ?srq_asserted(StatusByte) ->
    case StatusByte of
        _MsgAvail when ?mav_asserted(StatusByte) ->
            {retrieve_data, StateData};
        Err when Err =:= 244; Err =:= 144; Err =:= 192; Err =:= 208 ->
            {retrieve_error, <<"err?">>, StateData};
        ESR when ESR =:= 96; ESR =:= 112 ->
            {fetch_esr, <<"*esr?">>, StateData}
    end;
handle_stb(StatusByte, StateData) ->
    case StatusByte of
        Err when Err =:= 160; Err =:= 80; Err =:= 128; Err =:=144 ->
            {retrieve_err, <<"err?">>, StateData};
        ESR when ESR =:= 32; ESR =:= 48 ->
            {fetch_esr, <<"*esr?">>, StateData};
        _MsgAvail when ?mav_asserted(StatusByte) ->
            {retrieve_data, StateData}
    end.

handle_esr(1, StateData) ->
    {op_complete, StateData};
handle_esr(Err, StateData) when Err =:= 8; Err =:= 33; Err =:= 9; Err =:= 32 ->
    {retrieve_error, <<"syst:err?">>, StateData}.

handle_get(_AnyChannel, StateData) ->
    lager:warning("The hp8657b does not support get"),
    {error, {unsupported_method, read}, StateData}.

handle_set(power_level, NewValue, StateData) ->
    {send, [<<"AP">>, NewValue, <<"DM">>], StateData};
handle_set(cw_freq, NewValue, StateData) ->
    {send, [<<"FR">>, NewValue, <<"HZ">>], StateData}.
