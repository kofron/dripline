% dripline_backplane.erl
% written by jared kofron <jared.kofron@gmail.com>
% NIF interface to the I/O server backplane.  Allows for scanning
% of the bus for cards and initialization routines for the I/O carrier.
-module(dripline_backplane).
-export([scan/0]).
-on_load(init/0).

% dialyzer type specifications.  
-type card_slot() :: cardA | cardB | cardC | cardD.
-type card_model() :: ios320 | ios330 | ios408.

% loads the NIF shared library for the backplane.
-spec init() -> ok | term().
init() ->
    erlang:load_nif("priv/dripline_backplane",0).

% scan the I/O server backplane for installed cards.  
-spec scan() -> [{card_slot(),card_model()}].
scan() ->
    erlang:error("NIF library not loaded!").
