% ami420.erl
% @author jared kofron <jared.kofron@gmail.com>
% @doc A frontend module for the American Magnetics Power Supply Controller
% model 420.  Currently only supports reading current and voltage from the
% supply, as well as the coil constant.
-module(ami420).
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

handle_get(supply_current, StateData) ->
    {send, <<"CURR:MAG?">>, StateData};
handle_get(supply_voltage, StateData) ->
    {send, <<"VOLT:SUPP?">>, StateData};
handle_get(coil_constant, StateData) ->
    {send, <<"COIL?">>, StateData};
handle_get(magnetic_field, StateData) ->
    {send, <<"FIELD:MAG?">>, StateData};
handle_get(ramp_state, StateData) ->
    {send, <<"STATE?">>, StateData};
handle_get(persistent_switch, StateData) ->
    {send, <<"PSwitch?">>, StateData}.

handle_set(_AnyChannel, _NewValue, StateData) ->
    {error, {unsupported_method, write}, StateData}.
