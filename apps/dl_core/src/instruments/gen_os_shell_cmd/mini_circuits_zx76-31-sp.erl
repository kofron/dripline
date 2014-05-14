%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc
%%% @end
%%% Created : 9 May 2014 by Ben LaRoque
%%%-------------------------------------------------------------------
-module('mini_circuits_zx76-31-sp').
-behavior(gen_instrument).

-export([init/1,
         start_link/1,
         handle_get/2,
         handle_set/3]).

-record(state, {locator}).

%%--------------------------------------------------------------------
%% gen_instrument Functions
%%--------------------------------------------------------------------
init(Args) ->
    lager:info("mini_circuits args: ~p", [Args]),
    {ok, #state{locator={0,0}}}.

start_link(ID) ->
    gen_os_shell_cmd:start_link(?MODULE, ID).

handle_get(attenuation, StateData) ->
    {error, {unsupported_get, device_is_write_only}, StateData};
handle_get(ChName, StateData) ->
    {error, {unsupported_get, {no_locator, ChName}}, StateData}.

handle_set(attenuation, Value, StateData) ->
    lager:info("setting attenuator"),
    lager:info("calling:~n~p",[assemble_atten_cmd(Value, StateData)]),
    {send, assemble_atten_cmd(Value, StateData), StateData};
handle_set(ChName, _Value, StateData) ->
    {error, {unsupported_set, {no_locator, ChName}}, StateData}.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

assemble_atten_cmd(Value, #state{locator={Maj,Min}}) ->
    %FmtStr = "/home/laroque/virt_env/spidev/bin/python -c 'import spidev;spi=spidev.SpiDev();spi.open(~p,~p);spi.xfer([~p])'",
    FmtStr = "/usr/bin/python -c 'import datetime;foo=~p+~p;bar=~p;datetime.datetime.now()'",
    io_lib:format(FmtStr, [Maj, Min, erlang:binary_to_list(Value)]).%,
    %io_lib:format("/usr/bin/python -c 'import datetime;~p+~p'", [Maj, Min]).
