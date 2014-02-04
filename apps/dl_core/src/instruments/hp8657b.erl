%% @doc the Hewlett-Packard HP8657B is a low frequency sweeper.  It does not
%%      support reading its parameters.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(hp8657b).
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

handle_get(_AnyChannel, StateData) ->
    {error, {unsupported_method, read}, StateData}.

handle_set(power_level, NewValue, StateData) ->
    {send, [<<"AP">>,NewValue,<<"DM">>], StateData};
handle_set(cw_freq, NewValue, StateData) ->
    {send, [<<"FR">>,NewValue,<<"MZ">>], StateData}.
