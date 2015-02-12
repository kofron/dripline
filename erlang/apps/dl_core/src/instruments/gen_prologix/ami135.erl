%% @doc The AMI135 is a sample-and-hold liquid helium level 
%% sensor monitor.
-module(ami135).
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

handle_get('level', StateData) ->
    {send, <<"LEVEL">>, StateData}.
handle_set(_AnyChannel, _AnyValue, StateData) ->
    {error, {unsupported_method, write}, StateData}.


