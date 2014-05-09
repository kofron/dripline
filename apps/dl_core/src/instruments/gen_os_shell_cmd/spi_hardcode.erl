% disk_usage.erl
% Gets the current disk usage on a UNIX box - i.e. assumes
% that it can use du to get the usage.
-module(spi_hardcode).
-behavior(gen_os_cmd_spi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_os_cmd callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1,start_link/1]).
-export([base_cmd/0,process_args/2]).

%%%%%%%%%%%%%%%%%%%%%%
%%% internal state %%%
%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(ID) ->
    gen_os_cmd:start_link(?MODULE,ID).

init(_Args) ->
    {ok, #state{}}.

base_cmd() ->
    "python -c 'import spidev;spi=spidev.SpiDev();spi.open(0,0);spi.xfer([10])'".

process_args([_DiskName|_Args], _StateData) ->
    [].
