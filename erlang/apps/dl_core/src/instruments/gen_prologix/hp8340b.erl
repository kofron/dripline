%% @doc the Hewlett-Packard HP8340B is a high frequency synthesized 
%%        sweeper controlled via GPIB.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(hp8340b).
-behavior(gen_prologix).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([handle_get/2, handle_set/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1,start_link/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal record defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(InstrumentID,EProID,GPIBAddress) ->
    gen_prologix:start_link(?MODULE, InstrumentID, EProID, GPIBAddress).

init(_Args) ->
    InitialState = #state{},
    {ok, InitialState}.

handle_get(cw_freq, StateData) ->
    {send, <<"OPCW">>, StateData};
handle_get(power_level, StateData) ->
    {send, <<"OPPL">>, StateData};
handle_get(sweep_start_freq, StateData) ->
    {send, <<"OPFA">>, StateData};
handle_get(sweep_stop_freq, StateData) ->
    {send, <<"OPFB">>, StateData};
handle_get(sweep_time, StateData) ->
    {send, <<"OPST">>, StateData};
handle_get(AnyOther, StateData) ->
    {error, {unknown_channel, AnyOther}, StateData}.

handle_set(cw_freq, <<"disable">>, StateData) ->
    {send, [<<"RF0">>], StateData};
handle_set(cw_freq, <<"enable">>, StateData) ->
    {send, [<<"RF1">>], StateData};
handle_set(cw_freq, NewValue, StateData) ->
    {send, [<<"CW">>, NewValue, <<"MZ">>], StateData};
handle_set(power_level, NewValue, StateData) ->
    {send, [<<"PL">>, NewValue, <<"DB">>], StateData};
handle_set(sweep_start_freq, NewValue, StateData) ->
    {send, [<<"FA">>, NewValue, <<"MZ">>], StateData};
handle_set(sweep_stop_freq, NewValue, StateData) ->
    {send, [<<"FB">>, NewValue, <<"MZ">>], StateData};
handle_set(sweep_time, NewValue, StateData) ->
    {send, [<<"ST">>, NewValue, <<"MS">>], StateData};
handle_set(AnyOther, _NewValue, StateData) ->
    {error, {unknown_channel, AnyOther}, StateData}.
