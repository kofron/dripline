%% @doc the lecroy 9210 is a two channel pulser mainframe with support for two 
%% modules A and B.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(lecroy9210).
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

handle_set(trigger, _Value, StateData) ->
    {send, <<"*TRG">>, StateData};
handle_set(a_amplitude, Value, StateData) ->
    {send, [<<"A:AMP">>,<<" ">>,Value], StateData}.
handle_get(a_amplitude, StateData) ->
    {send, <<"A:AMP?">>, StateData}.

