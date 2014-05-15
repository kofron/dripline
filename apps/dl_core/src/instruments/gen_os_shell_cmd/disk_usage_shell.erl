%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc
%%% @end
%%% Created : 9 May 2014 by Ben LaRoque
%%%-------------------------------------------------------------------
-module(disk_usage_shell).
-behavior(dl_gen_instrument).

-export([init/1,
         start_link/1,
         handle_get/2,
         handle_set/3]).

-record(state, {locator}).

%%--------------------------------------------------------------------
%% dl_gen_instrument Functions
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

start_link(ID) ->
    gen_os_shell_cmd:start_link(?MODULE, ID).

handle_get(disk_usage, StateData) ->
    lager:warning("depricated usage"),
    {error, {deprecated_usage, {missing_info, device_name}}, StateData};
handle_get(Locator, StateData) ->
    case parse_as_device(Locator) of
        {ok, Device} ->
            {send, assemble_df_cmd(Device), StateData};
        nomatch ->
            lager:warning("device atom illformed"),
            {error, {unsupported_get, {no_locator, Locator}}, StateData}
    end.

%%This set is only for debugging purposes
handle_set(disk_usage, Disk, State) ->
    lager:warning("Set should *NOT* be used to get disk usage, this is for testing only"),
    {send, assemble_df_cmd(Disk), State};
handle_set(ChName, _Value, StateData) ->
    lager:warning("unrecognized set"),
    {error, {unsupported_set, {no_locator, ChName}}, StateData}.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------
parse_as_device(Locator) ->
    LocList = atom_to_list(Locator),
    case lists:prefix("disk_usage:", LocList) of
        true ->
            {_, Device} = lists:split(length("disk_usage:"), LocList),
            {ok, Device};
        false ->
            nomatch
        end.

assemble_df_cmd(Disk) ->
    FmtStr = "/bin/df ~p",
    DiskList = case is_binary(Disk) of
        true ->    
            erlang:binary_to_list(Disk);
        false ->
            Disk
        end,
    io_lib:format(FmtStr, [DiskList]).
