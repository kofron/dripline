% dripline_backplane.erl
% written by jared kofron <jared.kofron@gmail.com>
% NIF interface to the I/O server backplane.  Allows for scanning
% of the bus for cards and initialization routines for the I/O carrier.
-module(dripline_backplane).
-export([start/0]).
-on_load(init/0).

init() ->
    erlang:load_nif("../priv/dripline_backplane",0).

start() ->
    ok.
