%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc
%%% @end
%%% Created : 9 May 2014 by Ben LaRoque
%%%-------------------------------------------------------------------
-module(disk_usage_shell).
-behavior(gen_instrument).

-export([init/1,
         start_link/1,
         handle_get/2,
         handle_set/3]).

-record(state, {locator}).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------
init(Args) ->
    {ok, #state{}}.

start_link(ID) ->
    gen_os_shell_cmd:start_link(?MODULE, ID).

handle_get({disk_usage}, StateData) ->
    lager:notice("getting disk_usage"),
    {reply, assemble_df_cmd("/dev/sda1"), StateData};
handle_get(ChName, StateData) ->
    lager:warning("unrecognized get"),
    {error, {unsupported_get, {no_locator, ChName}}, StateData}.

handle_set(ChName, _Value, StateData) ->
    lager:warning("unrecognized set"),
    {error, {unsupported_set, {no_locator, ChName}}, StateData}.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

assemble_df_cmd(Disk) ->
    FmtStr = "/bin/df ~p",
    io_lib:format(FmtStr, [Disk]).
