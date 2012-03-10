%% @doc dripline_util contains functions that are used all over
%%		the code base as helpers.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dripline_util).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data munging functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([binary_to_atom/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CouchDB interface functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([update_couch_doc/4,strip_rev_no/1]).

%%---------------------------------------------------------------------%%
%% @doc binary_to_atom simply converts a binary string into an atom.
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
	erlang:list_to_atom(erlang:binary_to_list(Binary)).

%%---------------------------------------------------------------------%%
%% @doc update_couch_doc/4 updates a couch document *that already exists*
%%		with either a new or existing field by adding a line of data to
%%		that field.
%% @end
%%---------------------------------------------------------------------%%
-spec update_couch_doc(string(),binary(),string(),ejson:ejson_object()) 
		-> ok | {error,term()}.
update_couch_doc(DbName,DocID,FieldName,NewData) ->
	S = dripline_conn_mgr:get(),
	{ok,Db} = couchbeam:open_db(S,DbName),
	{ok,Doc} = couchbeam:open_doc(Db,DocID),
	EncodedData = ejson:encode(NewData),
	NewDoc = couchbeam_doc:set_value(FieldName,EncodedData,Doc),
	OldRevNo = strip_rev_no(couchbeam_doc:get_rev(NewDoc)),
	dripline_cmd_mon:notify(DocID, OldRevNo + 1),
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