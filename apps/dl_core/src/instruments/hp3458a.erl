%% @doc the Hewlett-Packard HP3458A is an 8 1/2 digit digital multimeter.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(hp3458a).
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

handle_get(dcv, StateData) ->
    {send, <<"DCV?">>, StateData}.

handle_set(_AnyChannel, _NewValue, StateData) ->
    {error, {unsupported_method, write}, StateData}.
