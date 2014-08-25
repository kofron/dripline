% px1500.erl
% a frontend module for the Signatec PX1500 digitizer as controlled
% by Mantis.  Exports the digitizer behavior as a gen_os_cmd.
-module(px1500).
-behavior(gen_os_cmd).

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
    "/usr/local/bin/mantis_client".

process_args(Args, _StateData) ->
    proplist_to_args(Args, []).

proplist_to_args([],Acc) ->
    lists:reverse(Acc);
proplist_to_args([{duration, D}|T], Acc) ->
    Str = lists:concat(["duration/d=", D]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{rate, R}|T], Acc) ->
    Str = lists:concat(["rate/d=", R]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{output, O}|T], Acc) ->
    Str = lists:concat(["file/s=",O]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{file, O}|T], Acc) ->
    Str = lists:concat(["file=/s",O]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{mode, M}|T], Acc) ->
    Str = lists:concat(["mode/i=", M]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{description, D}|T],Acc) ->
    Str = lists:concat(["description/s=",D]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{host, H}|T], Acc) ->
    Str = lists:concat(["host/s=", H]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{port, P}|T], Acc) ->
    Str = lists:concat(["host/s=", P]),
    proplist_to_args(T,[Str | Acc]).
