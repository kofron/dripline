%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc
%%% @end
%%% Created : 9 May 2014 by Ben LaRoque
%%%-------------------------------------------------------------------
-module('mini_circuits_zx76-31-sp').
-behavior(gen_instrument).

-export([handle_get/2,
         handle_set/3]).

-record(state, {locator}).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------

handle_get(attenuation, StateData) ->
    {error, {unsupported_get, device_is_write_only}, StateData};
handle_get(ChName, StateData) ->
    {error, {unsupported_get, {no_locator, ChName}}, StateData}.

handle_set(attenuation, Value, StateData) ->
    {send, assemble_atten_cmd(Value, StateData), StateData};
handle_set(ChName, _Value, StateData) ->
    {error, {unsupported_set, {no_locator, ChName}}, StateData}.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

assemble_atten_cmd(Value, #state{locator={Maj,Min}}) ->
    FmtStr = "python -c 'import spidev;spi=spidev.SpiDev();spi.open(~p,~p);spi.xfer([~p])'",
    io_lib:format(FmtStr, [Maj, Min, Value]).
