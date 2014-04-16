%%%-------------------------------------------------------------------
%%% @author Jared Kofron <jared.kofron@gmail.com>
%%% @copyright (C) 2014, Jared Kofron
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2014 by Jared Kofron <jared.kofron@gmail.com>
%%%-------------------------------------------------------------------
-module(lecroy9210).
-behavior(gen_gpib_spoller).

-export([start_link/4,
	 init/1,
	 handle_stb/2,
	 handle_esr/2,
	 sre_register_bitmask/1,
	 ese_register_bitmask/1]).

-record(state, {}).

-define(srq_asserted(X), ((X bsr 6) band 1) == 1).
-define(mav_asserted(X), ((X bsr 4) band 1) == 1).

sre_register_bitmask(_) ->
    176.
ese_register_bitmask(_) ->
    1.

init([]) ->
    {ok, #state{}}.

start_link(InstrName, GPIBAddress, BusMod, BusName) ->
    gen_gpib_spoller:start_link(?MODULE, InstrName, GPIBAddress, BusMod, BusName).

%% 
%% If there is a message available, that is the highest priority. 
%% Otherwise, we retrieve errors or clear the status byte as necessary.
%%
handle_stb(StatusByte, StateData) when ?srq_asserted(StatusByte) ->
    case StatusByte of
	_MsgAvail when ?mav_asserted(StatusByte) ->
	    {retrieve_data, StateData};
	Err when Err =:= 224; Err =:= 144; Err =:= 192; Err =:= 208 ->
	    {retrieve_error, <<"err?">>, StateData};
	ESR when ESR =:= 96; ESR =:= 112 ->
	    {clear_esr, <<"*esr?">>, StateData}
    end;
handle_stb(StatusByte, StateData) ->
    case StatusByte of
	Err when Err =:= 160; Err =:= 80; Err =:= 128; Err =:= 144 ->
	    {retrieve_error, <<"err?">>, StateData};
	ESR when ESR =:= 32; ESR =:= 48 ->
	    {clear_esr, <<"*esr?">>, StateData};
	_MsgAvail when ?mav_asserted(StatusByte) ->
	    {retrieve_data, StateData}
    end.

handle_esr(1, StateData) ->
    {op_complete, StateData}.
	
	

