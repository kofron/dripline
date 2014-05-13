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

handle_get(disk_usage, StateData) ->
    {reply, assemble_df_cmd("/dev/sda1"), StateData};
handle_get(ChName, StateData) ->
    lager:warning("unrecognized get"),
    {error, {unsupported_get, {no_locator, ChName}}, StateData}.

%%This set is only for debugging purposes
handle_set(disk_usage, Disk, State) ->
    lager:warning("Set should *NOT* be used to get disk usage, this is for testing only")
    {reply, assemble_df_cmd(Disk), State};
handle_set(ChName, _Value, StateData) ->
    lager:warning("unrecognized set"),
    {error, {unsupported_set, {no_locator, ChName}}, StateData}.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

assemble_df_cmd(Disk) ->
    FmtStr = "/bin/df ~p",
    DiskList = case is_binary(Disk) of
        true ->    
            erlang:binary_to_list(Disk);
        false ->
            Disk
        end,
    io_lib:format(FmtStr, [DiskList]).
