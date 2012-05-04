%% @doc dripline_util contains functions that are used all over
%%		the code base as helpers.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dripline_util).
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Timestamp functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([make_ts/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data munging functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([binary_to_atom/1, binary_to_float/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CouchDB interface functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([update_couch_doc/3,strip_rev_no/1]).

%%---------------------------------------------------------------------%%
%% @doc make_ts converts the current time as reported by the erlang VM
%%      into the external timestamp format (a string).
%% @end
%%---------------------------------------------------------------------%%
-spec make_ts() -> binary().
make_ts() ->
    LocalTime = calendar:local_time(),
    to_binary_ts(LocalTime).

-spec to_binary_ts(calendar:datetime()) -> binary().
to_binary_ts({{Y,M,D},{HH,MM,SS}}) ->
    FormStr = "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    L = lists:flatten(io_lib:format(FormStr,[Y,M,D,HH,MM,SS])),
    list_to_binary(L).

%%---------------------------------------------------------------------%%
%% @doc binary_to_atom simply converts a binary string into an atom.
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
	erlang:list_to_atom(erlang:binary_to_list(Binary)).

%%---------------------------------------------------------------------%%
%% @doc binary_to_float converts a binary string with a floating point
%%      value into that floating point value.
%%      e.g. <<"1.4E-2">> -> 0.014
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_float(binary()) -> float().
binary_to_float(Binary) ->
	erlang:list_to_float(erlang:binary_to_list(Binary)).

%%---------------------------------------------------------------------%%
%% @doc update_couch_doc/4 updates a couch document *that already exists*
%%		with either a new or existing field by adding a line of data to
%%		that field.
%% @end
%%---------------------------------------------------------------------%%
-spec update_couch_doc(string(),binary(),[{binary(), binary()}]) 
		-> ok | {error,term()}.
update_couch_doc(DbName,DocID,Props) ->
    S = dripline_conn_mgr:get(),
    {ok,Db} = couchbeam:open_db(S,DbName),
    {ok,Doc} = couchbeam:open_doc(Db,DocID),
    NewDoc = couchbeam_doc:extend(Props,Doc),
    OldRevNo = strip_rev_no(props:get('_rev',NewDoc)),
    NotifyMod = case DbName of 
		    "dripline_cmd" ->
			dripline_cmd_mon;
		    "dripline_conf" ->
			dripline_conf_mon
		end,
    NotifyMod:notify(DocID, OldRevNo + 1),
    {ok,_} = couchbeam:save_doc(Db,NewDoc).

%%---------------------------------------------------------------------%%
%% @doc strip_rev_no/1 takes a binary "_rev" tag and strips the revision
%% 		sequence number.  this is very useful for notifying the monitor
%%		that a sequence number is about to be changed by e.g. us.
%% @end
%%---------------------------------------------------------------------%%
-spec strip_rev_no(binary()) -> integer().
strip_rev_no(BinRev) ->
    [NS,_] = string:tokens(binary_to_list(BinRev),"-"),
    {N,[]} = string:to_integer(NS),
    N.
